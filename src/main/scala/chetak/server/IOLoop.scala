package chetak.server

import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}

/**
  * Created by tushark on 15/1/17.
  */
class IOLoop(selector: Selector, protocol: TcpProtocol)  extends Runnable {

  var running = false

  def debug(str: String) = {
    //println(str)
  }

  override def run(): Unit = {
    running = true

    while(running) {
      debug("Selecting.....")
      selector.select()
      debug("Selected.....")
      val keys = selector.selectedKeys().iterator()

      while (keys.hasNext()){
        val key = keys.next()
        // remove the key so that we don't process this OPERATION again.
        keys.remove()


        if (key.isValid && key.isAcceptable()){
          debug("Accepting connection")
          accept(key)
        }

        if (key.isValid && key.isWritable()){
          write(key)
        }

        if (key.isValid && key.isReadable()){
          read(key)
        }
      }
    }
  }

  def accept(key: SelectionKey) = {
    val serverSocketChannel = key.channel().asInstanceOf[ServerSocketChannel]
    val socketChannel = serverSocketChannel.accept()
    socketChannel.configureBlocking(false)
    protocol.newChannel(socketChannel)
    socketChannel.register(selector, protocol.defaultOpts(), null /*protocol.attachment()*/)
    debug("Channel accepted...")
  }

  def read(key: SelectionKey) = {
    debug("Reading connection")
    val channel = key.channel().asInstanceOf[SocketChannel]
    val readBuffer = protocol.buffer()
    readBuffer.clear()
    try {
      val read = channel.read(readBuffer)
      if (read == -1){
        protocol.closeChannel(key)
      }
      readBuffer.flip()
      protocol.read(key, readBuffer, read)
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

  def write(key: SelectionKey) = {
    debug("Writing...")
    protocol.write(key)
  }

  def stop() = {
    running = false
    //Break the event loop
    selector.wakeup()
  }
}
