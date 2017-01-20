package chetak.server

import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, SocketChannel}

/**
  * Created by tushark on 15/1/17.
  */
trait TcpProtocol {

  val DEFAULT_BUFFER_CAPACITY = 20*1024

  def defaultOpts() : Int = SelectionKey.OP_READ

  def buffer() : ByteBuffer = ByteBuffer.allocate(DEFAULT_BUFFER_CAPACITY)

  def newChannel(socketChannel: SocketChannel)

  def read(key: SelectionKey, buffer: ByteBuffer, bytesRead: Int)

  def write(key: SelectionKey)

  def closeChannel(key: SelectionKey) = {
    println("Nothing was there to be read, closing connection")
    val channel = key.channel().asInstanceOf[SocketChannel]
    channel.close()
    key.cancel()
  }

}
