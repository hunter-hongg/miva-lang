use std::collections::{HashMap, HashSet};

#[derive(Default)]
pub struct DependencyGraph {
    dependencies: HashMap<String, HashSet<String>>,
    dependents: HashMap<String, HashSet<String>>,
}

impl DependencyGraph {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_dependency(&mut self, node: &str, dep: &str) {
        self.dependencies
            .entry(node.to_string())
            .or_default()
            .insert(dep.to_string());
        self.dependencies.entry(dep.to_string()).or_default();

        self.dependents
            .entry(dep.to_string())
            .or_default()
            .insert(node.to_string());
        self.dependents.entry(node.to_string()).or_default();
    }

    pub fn get_all_dependents(&self, node: &str) -> HashSet<String> {
        let mut visited = HashSet::new();
        let mut stack = vec![node.to_string()];

        while let Some(current) = stack.pop() {
            if visited.insert(current.clone()) {
                if let Some(dependents) = self.dependents.get(&current) {
                    for dependent in dependents {
                        if !visited.contains(dependent) {
                            stack.push(dependent.clone());
                        }
                    }
                }
            }
        }

        visited.remove(node);
        visited
    }

}
