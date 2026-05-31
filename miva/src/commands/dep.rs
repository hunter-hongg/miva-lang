use super::color;

pub fn exec(_verbose: bool) -> anyhow::Result<()> {
    eprintln!("{}", color::errize("miva dep: not yet implemented (stub)"));
    Ok(())
}
