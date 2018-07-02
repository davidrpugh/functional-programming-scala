class HelloThread extends Thread {

  override def run(): Unit = {
    println("Hello my name is " + toString + "!")
    println("Pleased to meet you " + toString + "...")
  }

}
