use crate::data_structures::number_map::NumberMap;
use std::{
    collections::HashSet,
    fmt::{Debug, Formatter},
};

pub struct DirectedGraph<A> {
    pub offset: usize,
    next_node: usize,
    nodes: NumberMap<A>,
    edges: NumberMap<HashSet<usize>>,
}

impl<A> DirectedGraph<A> {
    pub fn add_node(&mut self, node: A) -> usize {
        let i = self.reserve_node();

        self.set_node(i, node);

        i
    }

    pub fn add_edge(&mut self, i: usize, j: usize) {
        self.edges.get_or_insert_with(i, HashSet::new).insert(j);
    }

    pub fn get_node(&self, i: usize) -> Option<&A> {
        self.nodes.get(i)
    }

    pub fn last(&self) -> Option<(usize, &A)> {
        self.nodes.last()
    }

    pub fn new(offset: usize) -> Self {
        Self {
            offset,
            next_node: offset,
            nodes: NumberMap::new(offset),
            edges: NumberMap::new(offset),
        }
    }

    pub fn reserve_node(&mut self) -> usize {
        let result = self.next_node;

        self.next_node += 1;

        result
    }

    pub fn set_node(&mut self, i: usize, node: A) {
        self.nodes.insert(i, node);
    }

    pub fn topological_sort(&self) -> TopologicalSort {
        let mut dependents = NumberMap::new(self.offset);

        for (i, dependencies) in self.edges.iter() {
            for &j in dependencies {
                dependents.get_or_insert_with(j, Vec::new).push(i);
            }
        }

        let mut dependency_counts = NumberMap::<usize>::new(self.offset);

        for (i, dependencies) in self.edges.iter() {
            dependency_counts.insert(i, dependencies.len());
        }

        for i in self.offset..self.next_node {
            if !dependency_counts.contains_key(i) {
                dependency_counts.insert(i, 0);
            }
        }

        let (mut leaves, isolated_nodes): (Vec<_>, Vec<_>) = dependency_counts
            .iter()
            .filter(|&(_, &count)| count == 0)
            .map(|(i, _)| i)
            .partition(|&i| {
                dependents
                    .get(i)
                    .is_some_and(|i_dependents| i_dependents.len() > 0)
            });

        let mut layers = Vec::new();

        while leaves.len() > 0 {
            layers.push(leaves);

            let mut new_leaves = Vec::new();

            for &i in &layers[layers.len() - 1] {
                if let Some(i_dependents) = dependents.get(i) {
                    for &j in i_dependents {
                        let count = dependency_counts.get_mut(j).unwrap();

                        *count -= 1;

                        if *count == 0 {
                            new_leaves.push(j);
                        }
                    }
                }
            }

            leaves = new_leaves;
        }

        TopologicalSort {
            layers,
            isolated_nodes,
        }
    }
}

impl<A: Debug> Debug for DirectedGraph<A> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("DirectedGraph")
            .field("nodes", &self.nodes)
            .field("edges", &self.edges)
            .finish()
    }
}

pub struct TopologicalSort {
    pub layers: Vec<Vec<usize>>,
    pub isolated_nodes: Vec<usize>,
}
