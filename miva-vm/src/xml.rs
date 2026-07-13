//! Hand-written XML parser and tree model for the Miva VM runtime.
//!
//! This mirrors the `std/xml` contract: a tree of `XmlNode`s rooted at a
//! `Document` node (kind 6). No external crates are used.

use std::sync::Arc;

/// Node kind, matching the `xml_kind` integer tags (0..6).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum XmlKind {
    Null = 0,
    Element = 1,
    Text = 2,
    Comment = 3,
    CData = 4,
    Pi = 5,
    Document = 6,
}

impl XmlKind {
    pub fn as_u8(self) -> u8 {
        self as u8
    }

    pub fn from_u8(v: u8) -> XmlKind {
        match v {
            1 => XmlKind::Element,
            2 => XmlKind::Text,
            3 => XmlKind::Comment,
            4 => XmlKind::CData,
            5 => XmlKind::Pi,
            6 => XmlKind::Document,
            _ => XmlKind::Null,
        }
    }
}

/// A single node in the parsed XML tree.
#[derive(Debug, Clone)]
pub struct XmlNode {
    pub kind: XmlKind,
    pub tag: String,
    pub attrs: Vec<(String, String)>,
    pub children: Vec<Arc<XmlNode>>,
    pub text: String,
    pub pi_target: String,
    pub pi_data: String,
}

impl XmlNode {
    pub fn new(kind: XmlKind) -> Self {
        XmlNode {
            kind,
            tag: String::new(),
            attrs: Vec::new(),
            children: Vec::new(),
            text: String::new(),
            pi_target: String::new(),
            pi_data: String::new(),
        }
    }
}

struct Parser {
    chars: Vec<char>,
    pos: usize,
}

impl Parser {
    fn new(s: &str) -> Self {
        Parser {
            chars: s.chars().collect(),
            pos: 0,
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.chars.get(self.pos).copied();
        if c.is_some() {
            self.pos += 1;
        }
        c
    }

    fn eof(&self) -> bool {
        self.pos >= self.chars.len()
    }

    fn starts_with(&self, s: &str) -> bool {
        let sc: Vec<char> = s.chars().collect();
        for (k, c) in sc.iter().enumerate() {
            if self.chars.get(self.pos + k) != Some(c) {
                return false;
            }
        }
        true
    }

    fn skip_ws(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    /// Decode XML entities: &amp; &lt; &gt; &quot; &apos; and numeric
    /// &#NNN; / &#xHHH;.
    fn decode_entities(&self, raw: &str) -> String {
        let chars: Vec<char> = raw.chars().collect();
        let mut out = String::new();
        let mut i = 0;
        while i < chars.len() {
            let c = chars[i];
            if c == '&' {
                let mut j = i + 1;
                while j < chars.len() && chars[j] != ';' {
                    j += 1;
                }
                if j < chars.len() {
                    let ent: String = chars[i + 1..j].iter().collect();
                    let decoded = if let Some(num) = ent
                        .strip_prefix("#x")
                        .or_else(|| ent.strip_prefix("#X"))
                    {
                        u32::from_str_radix(num, 16)
                            .ok()
                            .and_then(char::from_u32)
                            .unwrap_or(c)
                    } else if let Some(num) = ent.strip_prefix('#') {
                        num.parse::<u32>()
                            .ok()
                            .and_then(char::from_u32)
                            .unwrap_or(c)
                    } else {
                        match ent.as_str() {
                            "amp" => '&',
                            "lt" => '<',
                            "gt" => '>',
                            "quot" => '"',
                            "apos" => '\'',
                            _ => c,
                        }
                    };
                    out.push(decoded);
                    i = j + 1;
                } else {
                    out.push(c);
                    i += 1;
                }
            } else {
                out.push(c);
                i += 1;
            }
        }
        out
    }

    fn parse_name(&mut self) -> Result<String, String> {
        let mut name = String::new();
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == ':' || c == '-' || c == '_' || c == '.' {
                name.push(c);
                self.pos += 1;
            } else {
                break;
            }
        }
        if name.is_empty() {
            return Err("expected a name".into());
        }
        Ok(name)
    }

    fn parse_attributes(&mut self) -> Result<Vec<(String, String)>, String> {
        let mut attrs = Vec::new();
        loop {
            while let Some(c) = self.peek() {
                if c.is_whitespace() {
                    self.pos += 1;
                } else {
                    break;
                }
            }
            match self.peek() {
                None | Some('>') | Some('/') => break,
                Some(_) => {}
            }
            let name = self.parse_name()?;
            while let Some(c) = self.peek() {
                if c.is_whitespace() {
                    self.pos += 1;
                } else {
                    break;
                }
            }
            if self.peek() != Some('=') {
                return Err(format!("expected '=' after attribute '{}'", name));
            }
            self.bump();
            while let Some(c) = self.peek() {
                if c.is_whitespace() {
                    self.pos += 1;
                } else {
                    break;
                }
            }
            let q = match self.peek() {
                Some('"') | Some('\'') => self.bump().unwrap(),
                _ => return Err("expected attribute value quote".into()),
            };
            let mut val = String::new();
            while let Some(c) = self.peek() {
                if c == q {
                    self.bump();
                    break;
                }
                val.push(c);
                self.pos += 1;
            }
            let decoded = self.decode_entities(&val);
            attrs.push((name, decoded));
        }
        Ok(attrs)
    }

    fn parse_pi(&mut self) -> Result<XmlNode, String> {
        // consume "<?"
        self.bump();
        self.bump();
        let mut target = String::new();
        while let Some(c) = self.peek() {
            if c.is_whitespace() || c == '?' {
                break;
            }
            target.push(c);
            self.pos += 1;
        }
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.pos += 1;
            } else {
                break;
            }
        }
        let mut data = String::new();
        while !self.eof() {
            if self.starts_with("?>") {
                self.bump();
                self.bump();
                break;
            }
            data.push(self.peek().unwrap());
            self.pos += 1;
        }
        let mut node = XmlNode::new(XmlKind::Pi);
        node.pi_target = target;
        node.pi_data = data;
        Ok(node)
    }

    fn parse_comment(&mut self) -> Result<XmlNode, String> {
        // consume "<!--"
        for _ in 0..4 {
            self.bump();
        }
        let mut text = String::new();
        while !self.eof() {
            if self.starts_with("-->") {
                self.bump();
                self.bump();
                self.bump();
                break;
            }
            text.push(self.peek().unwrap());
            self.pos += 1;
        }
        let mut node = XmlNode::new(XmlKind::Comment);
        node.text = text;
        Ok(node)
    }

    fn parse_cdata(&mut self) -> Result<XmlNode, String> {
        // consume "<![CDATA["
        for _ in 0..9 {
            self.bump();
        }
        let mut text = String::new();
        while !self.eof() {
            if self.starts_with("]]>") {
                self.bump();
                self.bump();
                self.bump();
                break;
            }
            text.push(self.peek().unwrap());
            self.pos += 1;
        }
        let mut node = XmlNode::new(XmlKind::CData);
        node.text = text;
        Ok(node)
    }

    fn skip_declaration(&mut self) -> Result<(), String> {
        // consume "<!"
        self.bump();
        self.bump();
        let mut depth: i32 = 1;
        while !self.eof() {
            let c = self.peek().unwrap();
            if c == '"' || c == '\'' {
                let q = c;
                self.bump();
                while !self.eof() && self.peek() != Some(q) {
                    self.bump();
                }
                if !self.eof() {
                    self.bump();
                }
            } else if c == '<' {
                self.bump();
                depth += 1;
            } else if c == '>' {
                self.bump();
                depth -= 1;
                if depth == 0 {
                    break;
                }
            } else {
                self.bump();
            }
        }
        Ok(())
    }

    fn parse_element(&mut self) -> Result<XmlNode, String> {
        // consume "<"
        self.bump();
        let tag = self.parse_name()?;
        let attrs = self.parse_attributes()?;
        let self_closing = if self.starts_with("/>") {
            self.bump();
            self.bump();
            true
        } else if self.peek() == Some('>') {
            self.bump();
            false
        } else {
            return Err("expected '>' or '/>' in element".into());
        };

        let mut node = XmlNode::new(XmlKind::Element);
        node.tag = tag;
        node.attrs = attrs;
        if self_closing {
            return Ok(node);
        }

        loop {
            if self.eof() {
                return Err(format!("unexpected EOF in content of '{}'", node.tag));
            }
            if self.starts_with("</") {
                self.bump();
                self.bump();
                let endtag = self.parse_name()?;
                while let Some(c) = self.peek() {
                    if c == '>' {
                        self.bump();
                        break;
                    }
                    self.bump();
                }
                if endtag != node.tag {
                    return Err(format!(
                        "mismatched end tag: expected '{}', got '{}'",
                        node.tag, endtag
                    ));
                }
                break;
            } else if self.starts_with("<!--") {
                let c = self.parse_comment()?;
                node.children.push(Arc::new(c));
            } else if self.starts_with("<![CDATA[") {
                let cd = self.parse_cdata()?;
                node.children.push(Arc::new(cd));
            } else if self.starts_with("<?") {
                let pi = self.parse_pi()?;
                node.children.push(Arc::new(pi));
            } else if self.peek() == Some('<') {
                let child = self.parse_element()?;
                node.children.push(Arc::new(child));
            } else {
                let mut text = String::new();
                while let Some(c) = self.peek() {
                    if c == '<' {
                        break;
                    }
                    text.push(c);
                    self.pos += 1;
                }
                let decoded = self.decode_entities(&text);
                if !decoded.trim().is_empty() {
                    let mut tnode = XmlNode::new(XmlKind::Text);
                    tnode.text = decoded;
                    node.children.push(Arc::new(tnode));
                }
            }
        }
        Ok(node)
    }

    fn parse_document(&mut self) -> Result<XmlNode, String> {
        let mut doc = XmlNode::new(XmlKind::Document);
        loop {
            self.skip_ws();
            if self.eof() {
                break;
            }
            match self.peek() {
                Some('<') => {
                    if self.starts_with("<?") {
                        let pi = self.parse_pi()?;
                        doc.children.push(Arc::new(pi));
                    } else if self.starts_with("<!--") {
                        let c = self.parse_comment()?;
                        doc.children.push(Arc::new(c));
                    } else if self.starts_with("<![CDATA[") {
                        let cd = self.parse_cdata()?;
                        doc.children.push(Arc::new(cd));
                    } else if self.starts_with("<!") {
                        self.skip_declaration()?;
                    } else {
                        let el = self.parse_element()?;
                        doc.children.push(Arc::new(el));
                    }
                }
                _ => break,
            }
        }
        Ok(doc)
    }
}

/// Parse an XML document string into an `Arc<XmlNode>` `Document` root.
pub fn parse(input: &str) -> Result<Arc<XmlNode>, String> {
    let mut p = Parser::new(input);
    let doc = p.parse_document()?;
    Ok(Arc::new(doc))
}

fn escape_text(s: &str) -> String {
    let mut out = String::new();
    for c in s.chars() {
        match c {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            _ => out.push(c),
        }
    }
    out
}

fn escape_attr(s: &str) -> String {
    let mut out = String::new();
    for c in s.chars() {
        match c {
            '&' => out.push_str("&amp;"),
            '"' => out.push_str("&quot;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '\'' => out.push_str("&apos;"),
            _ => out.push(c),
        }
    }
    out
}

/// Serialize an `XmlNode` back to XML text.
pub fn stringify(node: &XmlNode) -> String {
    let mut out = String::new();
    stringify_into(node, &mut out);
    out
}

fn stringify_into(node: &XmlNode, out: &mut String) {
    match node.kind {
        XmlKind::Document => {
            for child in &node.children {
                stringify_into(child, out);
            }
        }
        XmlKind::Element => {
            out.push('<');
            out.push_str(&node.tag);
            for (k, v) in &node.attrs {
                out.push(' ');
                out.push_str(k);
                out.push_str("=\"");
                out.push_str(&escape_attr(v));
                out.push('"');
            }
            if node.children.is_empty() {
                out.push_str("/>");
            } else {
                out.push('>');
                for child in &node.children {
                    stringify_into(child, out);
                }
                out.push_str("</");
                out.push_str(&node.tag);
                out.push('>');
            }
        }
        XmlKind::Text => {
            out.push_str(&escape_text(&node.text));
        }
        XmlKind::Comment => {
            out.push_str("<!--");
            out.push_str(&escape_text(&node.text));
            out.push_str("-->");
        }
        XmlKind::CData => {
            out.push_str("<![CDATA[");
            out.push_str(&escape_text(&node.text));
            out.push_str("]]>");
        }
        XmlKind::Pi => {
            if node.pi_target == "xml" {
                out.push_str("<?xml ");
                out.push_str(&node.pi_data);
                out.push_str("?>");
            } else {
                out.push_str("<?");
                out.push_str(&node.pi_target);
                if !node.pi_data.is_empty() {
                    out.push(' ');
                    out.push_str(&node.pi_data);
                }
                out.push_str("?>");
            }
        }
        XmlKind::Null => {}
    }
}
