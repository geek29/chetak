package chetak

import chetak.server.http.HttpRequestMatcher._
import chetak.server.http._

/**
  * Created by tushark on 20/1/17.
  */
object EchoService extends HttpService {

  val echo = { req : HttpRequest =>
    Http.build(HttpStatusCodes.OK, StringBody(req.body.get.string), req)
  }


  override def routes: List[HttpRequestMatch] = List(path("/echo", List(post()(echo))))

}
