val t1 = new HelloThread
val t2 = new HelloThread

t1.start(); t2.start()
t1.join(); t2.join()