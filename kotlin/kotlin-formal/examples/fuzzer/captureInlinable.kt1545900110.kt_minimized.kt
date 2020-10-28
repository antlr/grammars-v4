inline fun <R> doWork(job: (() -> R)): R {
return notInline({job()})
}
inline fun <R> notInline(job: (() -> R)): R {
return (job)!!()
}