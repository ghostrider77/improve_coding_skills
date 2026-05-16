use std::cmp::{Ordering, Reverse};
use std::collections::BinaryHeap;
use std::io;

#[derive(Clone, Debug)]
struct JobSchedule {
    thread_id: usize,
    process_start_time: i64,
}

impl JobSchedule {
    fn new() -> Self {
        JobSchedule {thread_id: 0, process_start_time: 0}
    }
}

#[derive(Debug)]
struct ProcessedJob {
    thread_id: usize,
    job_id: usize,
    process_time: i64,
    finish_time: i64,
}

impl PartialEq for ProcessedJob {
    fn eq(&self, other: &Self) -> bool {
        self.thread_id == other.thread_id && self.finish_time == other.finish_time
    }
}

impl Eq for ProcessedJob {}

impl Ord for ProcessedJob {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.eq(other) {
            Ordering::Equal
        } else if (self.finish_time < other.finish_time) ||
                  (self.finish_time == other.finish_time && self.thread_id < other.thread_id) {
            Ordering::Less
        } else  {
            Ordering::Greater
        }
    }
}

impl PartialOrd for ProcessedJob {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn convert_to_int_vector(line: &str) -> Vec<i32> {
    line.split_whitespace()
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

fn create_initial_heap(initial_jobs: &[i32]) -> BinaryHeap<Reverse<ProcessedJob>> {
    let initial_size = initial_jobs.len();
    let mut heap = BinaryHeap::with_capacity(initial_size);
    for (job_id, process_time) in initial_jobs.iter().enumerate() {
        let job = ProcessedJob {
            thread_id: job_id,
            job_id,
            process_time: *process_time as i64,
            finish_time: *process_time as i64,
        };
        heap.push(Reverse(job));
    };

    heap
}

fn remove_remaining_elements(heap: &mut BinaryHeap<Reverse<ProcessedJob>>, processed_jobs: &mut [JobSchedule]) {
    while let Some(Reverse(ProcessedJob {thread_id, job_id, process_time, finish_time})) = heap.pop() {
        processed_jobs[job_id] = JobSchedule {thread_id, process_start_time: finish_time - process_time};
    }
}

fn process_jobs(job_processing_time: &Vec<i32>, nr_jobs: i32, nr_threads: i32) -> Vec<JobSchedule> {
    if nr_jobs <= nr_threads {
        return (0..nr_jobs).map(|id| JobSchedule {thread_id: id as usize, process_start_time: 0}).collect()
    }

    let mut jobs_processed_by_thread = vec![JobSchedule::new(); nr_jobs as usize];
    let (initial_jobs, remaining_jobs) = job_processing_time.split_at(nr_threads as usize);
    let mut heap = create_initial_heap(initial_jobs);
    for (process_time, job_id) in remaining_jobs.iter().zip(nr_threads..nr_jobs) {
        let Reverse(
            ProcessedJob {
                thread_id: free_thread_id,
                job_id: finished_job_id,
                process_time: finished_process_time,
                finish_time: finished_end_time,
            }
        ) = heap.pop().unwrap();
        jobs_processed_by_thread[finished_job_id] = JobSchedule {
            thread_id: free_thread_id, process_start_time: finished_end_time - finished_process_time,
        };
        let next_job = ProcessedJob {
            thread_id: free_thread_id,
            job_id: job_id as usize,
            process_time: *process_time as i64,
            finish_time: finished_end_time + *process_time as i64,
        };
        heap.push(Reverse(next_job));
    }
    remove_remaining_elements(&mut heap, &mut jobs_processed_by_thread);
    jobs_processed_by_thread
}

fn main() {
    let (nr_threads, nr_jobs) = read_pair();
    let job_processing_time = convert_to_int_vector(&read_line());
    let result = process_jobs(&job_processing_time, nr_jobs, nr_threads);
    result.iter().for_each(|JobSchedule {thread_id, process_start_time}| println!("{thread_id} {process_start_time}"));
}
