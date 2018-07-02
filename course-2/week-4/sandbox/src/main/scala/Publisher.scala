trait Publisher {

  def publish(): Unit = subscribers foreach (_.handler(this))

  def subscribe(subscriber: Subscriber): Unit = {
    subscribers += subscriber
  }

  def unSubscribe(subscriber: Subscriber): Unit = {
    subscribers -= subscriber
  }

  private[this] var subscribers: Set[Subscriber] = Set()

}
