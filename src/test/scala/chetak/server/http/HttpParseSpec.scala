package chetak.server.http

import chetak.json.{JObject, JString}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by tushark on 15/1/17.
  */
class HttpParseSpec extends FlatSpec with Matchers {

  val CRLF = "\r\n"
  val request1 = "POST /cgi-bin/process.cgi?param=1&&parm2=1233 HTTP/1.1" +
    CRLF + "User-Agent: Mozilla/4.0 (compatible; MSIE5.01; Windows NT)" +
    CRLF + 	"Host: www.tutorialspoint.com" +
    CRLF + 	"Content-Type: application/x-www-form-urlencoded" +
    CRLF + 	"Content-Length: 49" +
    CRLF + 	"Accept-Language: en-us" +
    CRLF + 	"Accept-Encoding: gzip, deflate" +
    CRLF + 	"Connection: Keep-Alive" +
    CRLF +
    CRLF + "licenseID=string&content=string&/paramsXML=string" +
    CRLF


  val jsonRequest = "POST /cgi-bin/process.cgi?param=1&&parm2=1233 HTTP/1.1" +
    CRLF + "User-Agent: Mozilla/4.0 (compatible; MSIE5.01; Windows NT)" +
    CRLF + 	"Host: www.tutorialspoint.com" +
    CRLF + 	"Content-Type: application/json" +
    CRLF + 	"Content-Length: 62" +
    CRLF + 	"Accept-Language: en-us" +
    CRLF + 	"Accept-Encoding: gzip, deflate" +
    CRLF + 	"Connection: Keep-Alive" +
    CRLF +
    CRLF + "{\"licenseID\":\"string\",\"content\":\"string\",\"paramsXML\":\"string\"}" +
    CRLF


  "Http parser" should "parse simple get/post request and http version" in {
    val request = Http.parse(request1)
    request.requestLine.method should be (HttpMethod.POST)
    request.requestLine.version should be (HttpVersion.VERSION_1_1)
  }

  it should "parse headers correctly" in {
    val request = Http.parse(request1)
    request.headers should contain (HttpHeader("user-agent", "Mozilla/4.0 (compatible; MSIE5.01; Windows NT)"))
    request.headers should contain (HttpHeader("host", "www.tutorialspoint.com"))
    request.headers should contain (HttpHeader("content-type", "application/x-www-form-urlencoded"))
    request.headers should contain (HttpHeader("content-length", "49"))
    request.headers should contain (HttpHeader("accept-language", "en-us"))
    request.headers should contain (HttpHeader("accept-encoding", "gzip, deflate"))
    request.headers should contain (HttpHeader("connection", "Keep-Alive"))
  }

  it should "parse query parameters" in {
    val request = Http.parse(request1)
    request.requestLine.queryParams should be (defined)
    import org.scalatest.PartialFunctionValues._
    request.requestLine.queryParams.get.valueAt("param") should equal ("1")
    request.requestLine.queryParams.get.valueAt("parm2") should equal ("1233")
  }

  it should "return body as whole as context-type form encoded are not supported yet" in {
    val request = Http.parse(request1)
    request.body should be (defined)
    val body = "licenseID=string&content=string&/paramsXML=string\r\n"
    request.body.get.string() should be (body)
  }

  it should "parse body as json when content type is specified as application/json" in {
    val request = Http.parse(jsonRequest)
    request.body should be (defined)
    val ct = request.headers.find(_.name == (HttpHeaders.CONTENT_TYPE))
    ct should be (defined)
    ct.get.value should be (MediaTypes.APPLICATION_JSON)
    val body = JSONBody(JObject(List(
      ("licenseID" -> JString("string")),
      ("content"-> JString("string")),
      ("paramsXML"-> JString("string")))))
    request.body.get should be (body)
  }

  "HttpResponse" should "generate valid tostring payload" in {
    val request = Http.parse(request1)
    val notimplementedHTML = "<html>\r\n" +
      "<body>\r\n" +
      "<h1>Not Implemented!</h1>\r\n" +
      "</body>\r\n" +
      "</html> \r\n"

    val response = Http.build(HttpStatusCodes.NotImplemented, StringBody(notimplementedHTML), request)

    response.toString should include ("HTTP/1.1 501 Not Implemented")
    response.toString should include (notimplementedHTML)
    response.toString should include ("Server: WhereIsMyDriver")
  }

}
