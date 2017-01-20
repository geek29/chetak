package chetak.server.http

/**
  * Created by tushark on 17/1/17.
  */
trait HttpService {

  def routes : List[HttpRequestMatch]

}
