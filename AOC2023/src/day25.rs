use nom::error::ErrorKind;
use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use nom::sequence::separated_pair;
use nom::multi::separated_list1;

use std::collections::{HashMap, HashSet};
use itertools::Itertools;
use multiset::HashMultiSet;
use rand::seq::IteratorRandom;
use rand::distributions::{Distribution, WeightedIndex};

use crate::day::{Day, DayNum};
use crate::utils::{parse, string_chunks};

type Graph = HashMap<String, HashMultiSet<String>>;

fn parse_line(line: &str) -> (String, HashMultiSet<String>) {
    let component_list = separated_list1(tag(" "), alpha1::<_, (_, ErrorKind)>);
    let components = separated_pair(alpha1, tag(": "), component_list);
    let (component_str, component_list_str) = parse(components, line);

    (component_str.to_string(), component_list_str.iter().map(|&s| s.to_string()).collect())
}

fn build_graph(components: &Vec<(String, HashMultiSet<String>)>) -> Graph {
    let mut graph: HashMap<String, HashMultiSet<String>> = HashMap::new();
    for (src, dst_vec) in components {
        for dst in dst_vec.iter() {
            graph.entry(src.clone()).or_insert(HashMultiSet::new()).insert(dst.clone());
            graph.entry(dst.clone()).or_insert(HashMultiSet::new()).insert(src.clone());
        }
    }

    graph
}

fn contract(graph: &mut Graph) {
    let v = graph.keys().choose(&mut rand::thread_rng()).unwrap().clone();

    let u_vector = graph[&v].iter().collect_vec();
    let weights = u_vector.clone().iter().map(|&s| graph[&v].count_of(s)).collect_vec();
    let dist = WeightedIndex::new(&weights).unwrap();
    let u = u_vector[dist.sample(&mut rand::thread_rng())].clone();

    let mut new_vertex = v.clone();
    new_vertex.push_str(&u);

    let v_iter = graph[&v].clone();
    let u_iter = graph[&u].clone();
    let chain = v_iter.iter()
        .chain(u_iter.iter())
        .filter(|&vertex| vertex != &u && vertex != &v);
    for vertex in chain {
        graph.entry(new_vertex.clone()).or_insert(HashMultiSet::new()).insert(vertex.clone());
        graph.entry(vertex.clone()).or_insert(HashMultiSet::new()).insert(new_vertex.clone());
        graph.entry(vertex.clone()).or_insert(HashMultiSet::new()).remove_all(&v);
        graph.entry(vertex.clone()).or_insert(HashMultiSet::new()).remove_all(&u);
    }

    graph.remove(&v);
    graph.remove(&u);
}

fn karger_algorithm(graph: &Graph, iterations: usize) -> (HashSet<String>, HashSet<String>) {
    let mut min_size = graph.clone().values().map(|v| v.len()).sum();
    let (mut min_a, mut min_b) = (HashSet::new(), HashSet::new());


    let mut iter = 0;
    while iter < iterations || (min_a.len() * min_b.len()) == graph.len() - 1 {
        let mut current_graph = graph.clone();

        while current_graph.len() > 2 {
            contract(&mut current_graph);
        }

        let mut graph_iter = current_graph.into_iter();
        let (s1, a) = graph_iter.next().unwrap();
        let (s2, _) = graph_iter.next().unwrap();

        let cut_size = a.len();
        if cut_size < min_size {
            min_size = cut_size;
            min_a = HashSet::from_iter(string_chunks(s1, 3));
            min_b = HashSet::from_iter(string_chunks(s2, 3));
        }
        iter += 1;
    }

    (min_a, min_b)
}

pub struct Day25 {}

impl Day for Day25 {
    type Input = Vec<(String, HashMultiSet<String>)>;
    type Result = usize;

    fn day(&self) -> DayNum { 25 }

    fn parse_input(&self, file_input: String) -> Self::Input {
        file_input.split('\n').map(parse_line).collect()
    }

    fn solve1(&self, input: Self::Input) -> Self::Result {
        let graph = build_graph(&input);
        let (min_a, min_b) = karger_algorithm(&graph, 20);

        min_a.len() * min_b.len()
    }

    fn solve2(&self, _input: Self::Input) -> Self::Result {
        42
    }
}

#[cfg(test)]
mod tests {
    use crate::day25::Day25;
    use crate::day::Day;
    use indoc::indoc;

    const INPUT: &str = indoc! {
        "jqt: rhn xhk nvd
         rsh: frs pzl lsr
         xhk: hfx
         cmg: qnr nvd lhk bvb
         rhn: xhk bvb hfx
         bvb: xhk hfx
         pzl: lsr hfx nvd
         qnr: nvd
         ntq: jqt hfx bvb xhk
         nvd: lhk
         lsr: lhk
         rzs: qnr cmg lsr rsh
         frs: qnr lhk lsr"};

    #[test]
    fn test25_1() {
        let day = Day25 {};
        assert_eq!(day.solve1(day.parse_input(INPUT.to_string())), 54);
    }

    #[test]
    fn test25_2() {
        let day = Day25 {};
        assert_eq!(day.solve2(day.parse_input2(INPUT.to_string())), 42);
    }
}