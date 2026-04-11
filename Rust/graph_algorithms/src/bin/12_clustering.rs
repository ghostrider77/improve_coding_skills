use std::io;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn distance(&self, other: &Self) -> f64 {
        ((self.x - other.x) as f64).hypot((self.y - other.y) as f64)
    }
}

struct PointDistance {
    p_ix: usize,
    q_ix: usize,
    distance: f64,
}

struct UnionFind {
    parents: Vec<usize>,
    ranks: Vec<i32>,
}

impl UnionFind {
    fn initialize(nr_points: i32) -> Self {
        let parents = (0..nr_points as usize).collect();
        let ranks = vec![0; nr_points as usize];
        UnionFind { parents, ranks }
    }

    fn find(&mut self, mut child_ix: usize) -> usize {
        let mut node_indices_on_path = Vec::new();
        loop {
            let parent_ix = self.parents[child_ix];
            if parent_ix == child_ix {
                node_indices_on_path.iter().for_each(|ix| self.parents[*ix] = parent_ix);
                return parent_ix
            }

            node_indices_on_path.push(child_ix);
            child_ix = parent_ix;
        }
    }

    fn union(&mut self, parent_ix_p: usize, parent_ix_q: usize) {
        if parent_ix_p != parent_ix_q {
            if self.ranks[parent_ix_p] > self.ranks[parent_ix_q] {
                self.parents[parent_ix_q] = parent_ix_p;
            } else {
                self.parents[parent_ix_p] = parent_ix_q;
                if self.ranks[parent_ix_p] == self.ranks[parent_ix_q] {
                    self.ranks[parent_ix_q] += 1;
                }
            }
        }
    }
}

fn convert_to_int_vector(line: &str) -> Vec<i32> {
    line
        .split_whitespace()
        .map(|x| x.parse().unwrap())
        .collect()
}

fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();

    line
}

fn read_pair() -> (i32, i32) {
    let line = read_line();
    let xs: Vec<i32> = convert_to_int_vector(&line);
    let [a, b] = xs.as_slice() else {
        panic!("Malformed input: {}", line);
    };
    (*a, *b)
}

fn read_points(nr_points: i32) -> Vec<Point> {
    let mut points = Vec::with_capacity(nr_points as usize);
    for _ in 0..nr_points {
        let (x, y) = read_pair();
        points.push(Point { x, y })
    }

    points
}

fn calc_sorted_pairwise_distances(points: &Vec<Point>) -> Vec<PointDistance> {
    let mut distances = Vec::new();
    for i in 0..points.len() {
        for j in (i + 1)..points.len() {
            let d = points[i].distance(&points[j]);
            distances.push(PointDistance { p_ix: i, q_ix: j, distance: d });
        }
    }

    distances
        .sort_by(|PointDistance { distance: d1, .. }, PointDistance { distance: d2, .. }| d1.partial_cmp(&d2).unwrap());
    distances
}

fn calc_optimal_clustering(nr_points: i32, points: &Vec<Point>, k: i32) -> f64 {
    let mut distances = calc_sorted_pairwise_distances(points).into_iter();
    let mut clusters = UnionFind::initialize(nr_points);
    let mut nr_clusters = nr_points;
    loop {
        let PointDistance { p_ix, q_ix, distance } = distances.next().unwrap();
        let cluster_of_p = clusters.find(p_ix);
        let cluster_of_q = clusters.find(q_ix);
        if cluster_of_p != cluster_of_q {
            clusters.union(cluster_of_p, cluster_of_q);
            if nr_clusters == k {
                return distance
            }
            nr_clusters -= 1;
        }
    }
}

fn main() {
    let nr_points = read_line().trim().parse().unwrap();
    let points = read_points(nr_points);
    let k = read_line().trim().parse().unwrap();
    let result = calc_optimal_clustering(nr_points, &points, k);
    println!("{}", result);
}
