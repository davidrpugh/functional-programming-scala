trait Simulation {

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val event = Event(currentTime + delay, () => block)
    agenda = insert(agenda, event)
  }

  type Action = () => Unit

  case class Event(time: Int, action: Action)

  private type Agenda = List[Event]
  private var agenda: Agenda = List()

  private def insert(ag: Agenda, e: Event): Agenda = ag match {
    case first :: rest if first.time <= e.time => first :: insert(rest, e)
    case _ => e :: ag
  }

  def currentTime: Int = time
  private var time: Int = 0

  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      time = first.time
      first.action()
      loop()
    case Nil =>
  }

  def run(): Unit = {
    afterDelay(0){ println("*** Simulation has started! Time is " + currentTime + ". ***") }
    loop()
  }

}
