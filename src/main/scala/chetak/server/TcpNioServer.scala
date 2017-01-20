package chetak.server

import java.net.InetSocketAddress
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel}

/**
  * Created by tushark on 15/1/17.
  */
class TcpNioServer(addr: InetSocketAddress, protocol: TcpProtocol) {

  val selector = Selector.open()
  val serverChannel = ServerSocketChannel.open()
  serverChannel.configureBlocking(false)
  serverChannel.socket().bind(addr)
  serverChannel.register(selector, SelectionKey.OP_ACCEPT)
  val loop = new IOLoop(selector, protocol)

  def start() = {
    val t = new Thread(loop)
    t.start()
  }

  def stop() = {
    loop.stop()
  }

}
