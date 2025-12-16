use crate::data_structures::number_map::NumberMap;
use std::{
    collections::HashSet,
    fmt::{Debug, Formatter},
    ops::Range,
};

pub(crate) struct DirectedGraph<A> {
    offset: usize,
    next_node: usize,
    nodes: NumberMap<A>,
    edges: NumberMap<HashSet<usize>>,
}

impl<A> DirectedGraph<A> {
    pub(crate) fn add_node(&mut self, node: A) -> (usize, &mut A) {
        let i = self.reserve_node();
        let node_reference = self.set_node(i, node);

        (i, node_reference)
    }

    pub(crate) fn add_edge(&mut self, i: usize, j: usize) {
        self.edges.get_or_insert_with(i, HashSet::new).insert(j);
    }

    pub(crate) fn edge_iter(&self) -> impl Iterator<Item = (usize, usize)> + use<'_, A> {
        self.edges
            .iter()
            .flat_map(|(i, edges)| edges.iter().map(move |&j| (i, j)))
    }

    pub(crate) fn get_next_node(&self) -> usize {
        self.next_node
    }

    pub(crate) fn get_node(&self, i: usize) -> Option<&A> {
        self.nodes.get(i)
    }

    pub(crate) fn get_offset(&self) -> usize {
        self.offset
    }

    pub(crate) fn new(offset: usize) -> Self {
        Self {
            offset,
            next_node: offset,
            nodes: NumberMap::new(offset),
            edges: NumberMap::new(offset),
        }
    }

    pub(crate) fn node_iter(&self) -> impl Iterator<Item = (usize, &A)> {
        self.nodes.iter()
    }

    pub(crate) fn number_reserved(&self) -> usize {
        self.next_node - self.offset
    }

    pub(crate) fn reserve_node(&mut self) -> usize {
        let result = self.next_node;

        self.next_node += 1;

        result
    }

    pub(crate) fn reserve_nodes(&mut self, count: usize) -> Range<usize> {
        let result = self.next_node..self.next_node + count;

        self.next_node += count;

        result
    }

    pub(crate) fn set_node(&mut self, i: usize, node: A) -> &mut A {
        self.nodes.insert(i, node)
    }

    pub(crate) fn topological_sort(&self) -> TopologicalSort {
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

        for (i, _) in self.nodes.iter() {
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
                    .is_some_and(|i_dependents| !i_dependents.is_empty())
            });

        let mut layers = Vec::new();

        while !leaves.is_empty() {
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

pub(crate) struct TopologicalSort {
    pub(crate) layers: Vec<Vec<usize>>,
    pub(crate) isolated_nodes: Vec<usize>,
}
