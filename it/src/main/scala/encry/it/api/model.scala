package encry.it.api

import org.asynchttpclient.{Request, Response}

case class UnexpectedStatusCodeException(request: Request, response: Response)
  extends Exception(s"Request: ${request.getUrl}\n Unexpected status code (${response.getStatusCode}): " +
    s"${response.getResponseBody}")

case class Peer(address: String, name: String, connectionType: String)