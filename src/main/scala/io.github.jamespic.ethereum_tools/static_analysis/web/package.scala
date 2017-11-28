package io.github.jamespic.ethereum_tools.static_analysis

import akka.http.scaladsl.marshallers.xml.ScalaXmlSupport
import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.model.MediaTypes
import akka.stream.scaladsl.Flow
import akka.util.ByteString

import scala.xml.NodeSeq
import scala.xml.dtd.DocType

package object web {
  implicit val htmlMarshaller: ToEntityMarshaller[NodeSeq] =
    ScalaXmlSupport.nodeSeqMarshaller(MediaTypes.`text/html`).map(
      entity => entity.transformDataBytes(Flow.fromFunction(bytes =>
        ByteString(DocType("html").toString) ++ bytes
      ))
    )
}
