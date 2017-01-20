package chetak.server.http

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by tushark on 15/1/17.
  */
class HttpRequestMatcherSpec extends FlatSpec with Matchers {

  val get = HttpRequestMatcher.get() {
    { req : HttpRequest =>
      Http.build(HttpStatusCodes.OK, StringBody("Hello Get for /mypath"), req)
    }
  }

  val put =  HttpRequestMatcher.put() {
    { req : HttpRequest =>
      Http.build(HttpStatusCodes.OK, StringBody("Hello put for /mypath"), req)
    }
  }

  val post =  HttpRequestMatcher.post() {
    { req : HttpRequest =>
      Http.build(HttpStatusCodes.OK, StringBody("Hello post for /mypath"), req)
    }
  }

  "HttpRequestMatcher" should "return default 501 response for empty route list" in {
    val request = HttpRequest(RequestLine(HttpMethod.GET, "/mypath", HttpVersion.VERSION_1_1, None), List(), Some(StringBody("")))
    val resp = HttpRequestMatcher.matchRequest(List(), request)
    resp.status should be (HttpStatusCodes.NotImplemented)
    resp should be (HttpRequestMatcher.DefaultHandler(request))

  }

  it should "execute path handler for given path" in {
    val route = HttpRequestMatcher.path("/cgi-bin/process.cgi", List(), { req : HttpRequest =>
      Http.build(HttpStatusCodes.OK, StringBody("Hello 8789 for /mypath"), req)
    })

    val request = HttpRequest(RequestLine(HttpMethod.GET, "/cgi-bin/process.cgi", HttpVersion.VERSION_1_1, None), List(), Some(StringBody("")))
    val resp = HttpRequestMatcher.matchRequest(List(route), request)
    resp.status should be (HttpStatusCodes.OK)
    resp should equal (Http.build(HttpStatusCodes.OK, StringBody("Hello 8789 for /mypath"), request))
  }

  it should "execute get/post/put handler for get/put/post methods" in {
    val request = HttpRequest(RequestLine(HttpMethod.GET, "/cgi-bin/process.cgi", HttpVersion.VERSION_1_1, None), List(), Some(StringBody("")))
    val resp = HttpRequestMatcher.matchRequest(List(get, put, post), request)
    resp.status should be (HttpStatusCodes.OK)
    resp should equal (Http.build(HttpStatusCodes.OK, StringBody("Hello Get for /mypath"), request))

    val request2 = HttpRequest(RequestLine(HttpMethod.PUT, "/cgi-bin/process.cgi", HttpVersion.VERSION_1_1, None), List(), Some(StringBody("")))
    val resp2 = HttpRequestMatcher.matchRequest(List(get, put, post), request2)
    resp2.status should be (HttpStatusCodes.OK)
    resp2 should equal (Http.build(HttpStatusCodes.OK, StringBody("Hello put for /mypath"), request2))

    val request3 = HttpRequest(RequestLine(HttpMethod.POST, "/cgi-bin/process.cgi", HttpVersion.VERSION_1_1, None), List(), Some(StringBody("")))
    val resp3 = HttpRequestMatcher.matchRequest(List(get, put, post), request3)
    resp3.status should be (HttpStatusCodes.OK)
    resp3 should equal (Http.build(HttpStatusCodes.OK, StringBody("Hello post for /mypath"), request3))
  }

  it should "execute inner get/post/put handlers for get/put/post methods for give path" in {
    val route = HttpRequestMatcher.path("/cgi-bin/process.cgi", List(get, put, post))

    val request = HttpRequest(RequestLine(HttpMethod.GET, "/cgi-bin/process.cgi", HttpVersion.VERSION_1_1, None), List(), Some(StringBody("")))
    val resp = HttpRequestMatcher.matchRequest(List(route), request)
    resp.status should be (HttpStatusCodes.OK)
    resp should equal (Http.build(HttpStatusCodes.OK, StringBody("Hello Get for /mypath"), request))

    val request2 = HttpRequest(RequestLine(HttpMethod.PUT, "/cgi-bin/process.cgi", HttpVersion.VERSION_1_1, None), List(), Some(StringBody("")))
    val resp2 = HttpRequestMatcher.matchRequest(List(route), request2)
    resp2.status should be (HttpStatusCodes.OK)
    resp2 should equal (Http.build(HttpStatusCodes.OK, StringBody("Hello put for /mypath"), request2))

    val request3 = HttpRequest(RequestLine(HttpMethod.POST, "/cgi-bin/process.cgi", HttpVersion.VERSION_1_1, None), List(), Some(StringBody("")))
    val resp3 = HttpRequestMatcher.matchRequest(List(route), request3)
    resp3.status should be (HttpStatusCodes.OK)
    resp3 should equal (Http.build(HttpStatusCodes.OK, StringBody("Hello post for /mypath"), request3))
  }

  it should "execute multiple inner routes" in {
    val route1 = HttpRequestMatcher.path("/cgi-bin/process.cgi", List(get, put, post))

    val post2 =  HttpRequestMatcher.post() {
      { req : HttpRequest =>
        Http.build(HttpStatusCodes.OK, StringBody("Hello post2 for cgi-bin2/process.cgi"), req)
      }
    }

    val route2 = HttpRequestMatcher.path("/cgi-bin2/process.cgi", List(post2))

    val request = HttpRequest(RequestLine(HttpMethod.GET, "/cgi-bin/process.cgi", HttpVersion.VERSION_1_1, None), List(), Some(StringBody("")))
    val resp = HttpRequestMatcher.matchRequest(List(route1, route2), request)
    resp.status should be (HttpStatusCodes.OK)
    resp should equal (Http.build(HttpStatusCodes.OK, StringBody("Hello Get for /mypath"), request))

    val request2 = HttpRequest(RequestLine(HttpMethod.POST, "/cgi-bin2/process.cgi", HttpVersion.VERSION_1_1, None), List(), Some(StringBody("")))
    val resp2 = HttpRequestMatcher.matchRequest(List(route1, route2), request2)
    resp2.status should be (HttpStatusCodes.OK)
    resp2 should equal (Http.build(HttpStatusCodes.OK, StringBody("Hello post2 for cgi-bin2/process.cgi"), request2))

    //unimplemented path request should return 501 in case of nested handlers
    val request3 = HttpRequest(RequestLine(HttpMethod.POST, "/xxx", HttpVersion.VERSION_1_1, None), List(), Some(StringBody("")))
    val resp3 = HttpRequestMatcher.matchRequest(List(route1, route2), request3)
    resp3.status should be (HttpStatusCodes.NotImplemented)
    resp3 should be (HttpRequestMatcher.DefaultHandler(request))
  }

  it should "return internal server error for handler throwing error" in {
    val route = HttpRequestMatcher.path("/cgi-bin/process.cgi", List(), { req : HttpRequest =>
      throw new RuntimeException("Internal Server Error")
    })

    val request = HttpRequest(RequestLine(HttpMethod.GET, "/cgi-bin/process.cgi", HttpVersion.VERSION_1_1, None), List(), Some(StringBody("")))
    val resp = HttpRequestMatcher.matchRequest(List(route), request)
    resp.status should be (HttpStatusCodes.InternalServerError)
  }



}
