using DataStructures


struct ProcessedJob
    threadid::Int
    jobid::Int
    processtime::Int
    finishtime::Int
end


Base.isless(p1::ProcessedJob, p2::ProcessedJob) =
    (p1.finishtime < p2.finishtime) || (p1.finishtime == p2.finishtime && p1.threadid < p2.threadid)


converttointlist(line) = map(x -> parse(Int, x), split(line))


function createheap(jobs)
    heap = BinaryMinHeap{ProcessedJob}()
    for (jobid, processtime) in enumerate(jobs)
        push!(heap, ProcessedJob(jobid, jobid, processtime, processtime))
    end
    heap
end


function empty(heap, processedjobs)
    while !isempty(heap)
        job = pop!(heap)
        processedjobs[job.jobid] = (job.threadid-1, job.finishtime-job.processtime)
    end
end


function processjobs(job_processingtime, nrjobs, nrthreads)
    if nrjobs <= nrthreads
        return map(threadid -> (threadid-1, 0), 1:nrjobs)
    end
    processedjobs = fill((0, 0), nrjobs)
    initialjobs, remainingjobs = job_processingtime[1:nrthreads], job_processingtime[nrthreads+1:end]
    heap = createheap(initialjobs)
    for (ix, processtime) in enumerate(remainingjobs)
        jobid = nrthreads + ix
        finishedjob = pop!(heap)
        processedjobs[finishedjob.jobid] = (finishedjob.threadid-1, finishedjob.finishtime - finishedjob.processtime)
        push!(heap, ProcessedJob(finishedjob.threadid, jobid, processtime, finishedjob.finishtime+processtime))
    end
    empty(heap, processedjobs)
    processedjobs
end


function main()
    nrthreads, nrjobs = readline() |> converttointlist
    job_processingtime = readline() |> converttointlist
    result = processjobs(job_processingtime, nrjobs, nrthreads)
    for item in result
        println(join(item, " "))
    end
end


main()
