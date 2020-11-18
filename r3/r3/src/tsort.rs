// chapter 4 needs a topological sorting
// Refer to https://blog.csdn.net/lisonglisonglisong/article/details/45543451
use std::collections::{HashMap, HashSet};

struct Graph {
    vertex: usize, //  顶点数
    adjacent: HashMap<usize, HashSet<usize>>,
    indegree: Vec<usize>,
}

impl Graph {
    pub fn new(n: usize) -> Self {
        let mut adjacent = HashMap::with_capacity(n);
        for i in 0..n {
            adjacent.insert(i, HashSet::new());
        }
        Graph { 
            vertex: n,
            adjacent,
            indegree: vec![0; n],
        }
    }
    pub fn add_edge(&mut self, v1: usize, v2: usize) {
        let mut set = self.adjacent.entry(v1).or_insert(HashSet::new());
        set.insert(v2);
        self.indegree[v2] += 1;
    }

    pub fn find_zero(&self) -> usize {
        for &k in self.adjacent.keys() {
            if self.indegree[k] == 0 {
                return k;
            }
        }
        panic!("No DAG");
    }

    pub fn tsort(&mut self) -> Vec<usize> {
        // find the zero indegreee
        // add it and remove edges
        // loop until not nodes
        let mut seq = Vec::new();
        while self.adjacent.len() > 0 {
            // find zero 
            let k = self.find_zero();
            // in seq
            seq.push(k);
            // remove from adjacent
            // decrease nodes indegree
            let ins = self.adjacent.get(&k).unwrap();
            for &i in ins.into_iter() {
                self.indegree[i] -= 1;
            }
            self.adjacent.remove(&k);
        }
        seq
    }
}

pub fn tsort(nodes: usize, edges: Vec<(usize, usize)>) -> Vec<usize> {
    let mut graph = Graph::new(nodes);
    for (v1, v2) in edges.into_iter() {
        graph.add_edge(v1, v2);
    }
    return graph.tsort();
}


// fn main()
// {
//     let v = 6;
//     let edges = vec![
//         (5, 2),
//         (5, 0),
//         (4, 0),
//         (4, 1),
//         (2, 3),
//         (3, 1),
//     ];
//     let t = tsort(v, edges);
//     println!("{:?}", t);
// }