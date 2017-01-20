package chetak

import java.net.InetSocketAddress

import chetak.server.TcpNioServer
import chetak.server.http.HttpTcp
/**
  * Created by tushark on 15/1/17.
  */
object Chetak {

  def main(args: Array[String]): Unit = {
    val port = if(args.isEmpty)
      1111
    else args(0).toInt
    val server = new TcpNioServer(new InetSocketAddress("localhost", port), new HttpTcp(List(EchoService)))
    server.start()
    println(s"Chetak Server started on ${port}")
    Thread.sleep(Long.MaxValue)
  }

}
