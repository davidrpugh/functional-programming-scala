val x = new AnyRef {}
private var uuid = 0L


def uniqueIdentifier(): Long = x.synchronized {
  uuid += 1
  uuid
}


def startThread(): Thread = {
  val t = new Thread {
    override def run(): Unit = {
      val uuids = for (i <- 0 until 10) yield uniqueIdentifier()
      println(uuids)
    }
  }
  t.start()
  t
}


val t1, t2 = startThread()
t1.join(); t2.join()
