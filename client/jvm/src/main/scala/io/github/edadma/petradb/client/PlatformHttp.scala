package io.github.edadma.petradb.client

import java.net.URI
import java.net.http.{HttpClient as JHttpClient, HttpRequest as JHttpRequest, HttpResponse as JHttpResponse}
import scala.concurrent.{Future, ExecutionContext}
import scala.jdk.FutureConverters.*

def platformPost(url: String, body: String, headers: Map[String, String])(implicit ec: ExecutionContext): Future[HttpResponse] =
  val client  = JHttpClient.newHttpClient()
  val builder = JHttpRequest.newBuilder()
    .uri(URI.create(url))
    .header("Content-Type", "application/json")
    .POST(JHttpRequest.BodyPublishers.ofString(body))
  headers.foreach { (k, v) => builder.header(k, v) }
  client.sendAsync(builder.build(), JHttpResponse.BodyHandlers.ofString()).asScala.map { resp =>
    HttpResponse(resp.statusCode(), resp.body())
  }

def platformDelete(url: String, headers: Map[String, String])(implicit ec: ExecutionContext): Future[HttpResponse] =
  val client  = JHttpClient.newHttpClient()
  val builder = JHttpRequest.newBuilder()
    .uri(URI.create(url))
    .DELETE()
  headers.foreach { (k, v) => builder.header(k, v) }
  client.sendAsync(builder.build(), JHttpResponse.BodyHandlers.ofString()).asScala.map { resp =>
    HttpResponse(resp.statusCode(), resp.body())
  }
