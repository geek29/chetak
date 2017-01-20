package chetak.server

import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, SocketChannel}

/**
  * Created by tushark on 15/1/17.
  */
class EchoTcp extends TcpProtocol {

  var echoLine = ""

  override def newChannel(socketChannel: SocketChannel): Unit = {
    println("New Client : "+ socketChannel)
  }

  override def read(key: SelectionKey, readBuffer: ByteBuffer, bytesRead: Int): Unit = {
    val data = new Array[Byte](1000)
    readBuffer.get(data, 0, bytesRead)
    echoLine = new String(data)
    println("Received: "+ echoLine)
    key.interestOps(SelectionKey.OP_WRITE)
  }

  override def write(key: SelectionKey): Unit = {
    val channel = key.channel().asInstanceOf[SocketChannel]
    channel.write(ByteBuffer.wrap((echoLine).getBytes))
    key.interestOps(SelectionKey.OP_READ)
  }
}
