package org.llm4s.imageprocessing.provider

import org.llm4s.imageprocessing.MediaType
import upickle.default.{ReadWriter => RW, _}

private[provider] case class OpenAIMessage(
    role: String = "user",
    content: ujson.Value
)

object OpenAIMessage {
  implicit val rw: RW[OpenAIMessage] = readwriter[ujson.Value].bimap[OpenAIMessage](
    msg => ujson.Obj("role" -> msg.role, "content" -> msg.content),
    json => OpenAIMessage(json("role").str, json("content"))
  )
}

private[provider] case class OpenAIRequestBody(
    model: String,
    messages: List[OpenAIMessage],
    max_tokens: Int
)

object OpenAIRequestBody {
  implicit val rw: RW[OpenAIRequestBody] = readwriter[ujson.Value].bimap[OpenAIRequestBody](
    request => {
      // Azure OpenAI requires max_completion_tokens for o1 reasoning models
      val tokenKey = if (request.model.startsWith("o1")) "max_completion_tokens" else "max_tokens"
      ujson.Obj(
        "model" -> request.model,
        "messages" -> writeJs(request.messages),
        tokenKey -> request.max_tokens
      )
    },
    json => {
      // Support reading both keys for backward compatibility
      val maxTokens = json.obj.get("max_completion_tokens")
        .orElse(json.obj.get("max_tokens"))
        .map(_.num.toInt)
        .getOrElse(throw new java.util.NoSuchElementException("Missing max_tokens or max_completion_tokens"))

      OpenAIRequestBody(
        model = json("model").str,
        messages = read[List[OpenAIMessage]](json("messages")),
        max_tokens = maxTokens
      )
    }
  )

  def serialize(
      model: String,
      maxTokens: Int,
      prompt: String,
      base64Image: String,
      mediaType: MediaType
  ): String = {
    val mimeType = mediaType match {
      case MediaType.Jpeg => "jpeg"
      case MediaType.Png  => "png"
      case MediaType.Gif  => "gif"
      case MediaType.WebP => "webp"
      case MediaType.Bmp  => "bmp"
      case MediaType.Tiff => "tiff"
    }

    val textContent = ujson.Obj(
      "type" -> "text",
      "text" -> prompt
    )

    val imageContent = ujson.Obj(
      "type" -> "image_url",
      "image_url" -> ujson.Obj(
        "url" -> s"data:image/$mimeType;base64,$base64Image"
      )
    )

    val requestBody = OpenAIRequestBody(
      model = model,
      messages = List(
        OpenAIMessage(
          content = ujson.Arr(textContent, imageContent)
        )
      ),
      max_tokens = maxTokens
    )

    write(requestBody)
  }
}