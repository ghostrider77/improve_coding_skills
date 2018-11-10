import sys
from collections import namedtuple
from heapq import heappush, heappop


class ProcessedJob(namedtuple("ProcessedJob", ["thread_id", "job_id", "process_time", "finish_time"])):
    def __lt__(self, other):
        return (self.finish_time < other.finish_time
                or (self.finish_time == other.finish_time and self.thread_id < other.thread_id))


def convert_to_intlist(line):
    return tuple([int(item) for item in line.split()])


def create_initial_heap(jobs):
    heap = []
    for job_id, process_time in enumerate(jobs):
        heappush(heap, ProcessedJob(job_id, job_id, process_time, process_time))
    return heap


def remove_remaining_elements_from_heap(heap, jobs_processed_by_thread):
    while heap:
        thread_id, job_id, process_time, finish_time = heappop(heap)
        jobs_processed_by_thread[job_id] = (thread_id, finish_time - process_time)


def process_jobs(job_processing_time, number_of_jobs, number_of_threads):
    if number_of_jobs <= number_of_threads:
        return [(thread_id, 0) for thread_id in range(number_of_jobs)]

    jobs_processed_by_thread = [(0, 0)] * number_of_jobs
    initial_jobs, remaining_jobs = job_processing_time[:number_of_threads], job_processing_time[number_of_threads:]
    heap = create_initial_heap(initial_jobs)
    for ix, process_time in enumerate(remaining_jobs):
        job_id = number_of_threads + ix
        free_thread_id, finished_job_id, finished_process_time, finished_end_time = heappop(heap)
        jobs_processed_by_thread[finished_job_id] = (free_thread_id, finished_end_time - finished_process_time)
        heappush(heap, ProcessedJob(free_thread_id, job_id, process_time, finished_end_time+process_time))

    remove_remaining_elements_from_heap(heap, jobs_processed_by_thread)
    return jobs_processed_by_thread


def main():
    reader = sys.stdin
    number_of_threads, number_of_jobs = convert_to_intlist(next(reader))
    job_processing_time = convert_to_intlist(next(reader))
    result = process_jobs(job_processing_time, number_of_jobs, number_of_threads)
    for item in result:
        print(" ".join([str(elem) for elem in item]))


if __name__ == "__main__":
    main()
