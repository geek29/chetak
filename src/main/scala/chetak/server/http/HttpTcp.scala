package chetak.server.http

import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, SocketChannel}

import chetak.server.TcpProtocol

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

/**
  * Created by tushark on 15/1/17.
  */
class HttpTcp(services: List[HttpService]) extends TcpProtocol {

  val routes = services.foldLeft(List[HttpRequestMatch]()) { (list, service) =>
    list ::: service.routes
  }

  override def newChannel(socketChannel: SocketChannel): Unit = {

  }

  //Here we assume requests are <= 20KB size so its read in one single Go
  //Typically requests will be read over multiple calls
  //But we are not implementing partial request reads
  override def read(key: SelectionKey, buffer: ByteBuffer, bytesRead: Int): Unit = {
    val data = new Array[Byte](DEFAULT_BUFFER_CAPACITY)
    buffer.get(data, 0, bytesRead)
    val echoLine = new String(data)
    val promise = Promise[String]()
    key.attach(promise)

    implicit val ec = ExecutionContext.Implicits.global
    val f = Future {
      val httpReq = Http.parse(echoLine)
      debug("HttpReq: " + httpReq)
      val httpResp = HttpRequestMatcher.matchRequest(routes, httpReq)
      promise.success(httpResp.toString)
      debug("HttpResp: " + httpResp)
      //println("Signalled Selector for write Event as Ready")
    }

    //Signal selector for WRITING HTTP RESPONSE
    f.onComplete { x =>
      key.interestOps(SelectionKey.OP_WRITE)
      key.selector().wakeup()
      debug("Signalled Selector for write Event as Ready")
    }
  }

  def debug(str: String) = {
    println(str)
  }

  var count = 0
  var notCount = 0

  override def write(key: SelectionKey): Unit = {
    val channel = key.channel().asInstanceOf[SocketChannel]
    val promise = key.attachment().asInstanceOf[Promise[String]]
    debug("Write is called..")
    if(promise.isCompleted) {
      val result = promise.future.value
      if(result.isDefined) {
        result.get match {
          case Success(string) =>
            channel.write(ByteBuffer.wrap(string.getBytes))
            channel.close()
          case Failure(ex) =>
            val errorString = Http.error(ex).toString
            channel.write(ByteBuffer.wrap(errorString.getBytes))
            channel.close()
        }
      }
    } else {
      debug("Not writing.....")
      //This wont happen as we are calling future complete first and then trigerring write call
      /*val errorString = Http.error(ex)
      channel.write(ByteBuffer.wrap(errorString.getBytes))
      channel.close()*/
    }
  }


}
