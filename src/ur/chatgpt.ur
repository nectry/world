open Json
open Urls

functor TwoLegged(M : sig
                        val api_token : string
                    end) = struct
    open M

    val token = return (Some api_token)
end

datatype role = System | User | Assistant

val read_role = mkRead' (fn s =>
                            case s of
                                "system" => Some System
                              | "user" => Some User
                              | "assistant" => Some Assistant
                              | _ => None)
                        "role"

val show_role = mkShow (fn x =>
                           case x of
                               System => "system"
                             | User => "user"
                             | Assistant => "assistant")

val json_role : json role = @json_derived Result.readResult show _

datatype message
  = PlainText of {Role : role, Content : string}
  | UserImageUrl of string
  | UserImage of {Image : blob, MimeType : string}

(* We're using a simplified message type, (as compared to
https://platform.openai.com/docs/api-reference/chat/create), where, in
particular, all correspondence from the user must be delivered in individual
messages as opposed to being grouped up as a list of content.  In the following
json instance, the list makes an appearance (so that data is serialized to
properly match the api spec), but perhaps it should be exposed?  More generally,
what's the difference between having two User messages in a row and one User
message with two elements in its `Content` list?  If nothing, then this is
totally fine! *)
val json_message : json message =
  let val messageJson : json {Role : role, Content : variant [Str = string, Lst = list {Typ : string, ImageUrl : {Url : string}}]} =
        let val _ : json {Url : string} =
              json_record {Url = "url"}
            val _ : json {Typ : string, ImageUrl : {Url : string}} =
              json_record {Typ = "type", ImageUrl = "image_url"}
            val _ : json (variant [Str = string, Lst = list {Typ : string, ImageUrl : {Url : string}}]) =
              json_variant_anon
        in json_record {Role = "role", Content = "content"}
        end
  in json_derived
    (fn x => match x.Content {
      Str = fn str => Success (PlainText {Role = x.Role, Content = str}),
      Lst = fn lst => case lst of
        (* How can we actually tell if we have a base64 image or an url?  It's
        sort of impossible.  What we can do is make a good guess based on "magic
        numbers" (see https://stackoverflow.com/a/58158656 for details).
        ChatGPT currently only supports gif, png, and jpg. *)
        (* TODO: It might be worth checking, say, the Role is correct or the Typ
        is indeed "image_url". *)
        (* That said, I don't think this code is ever used anyway as we never
        parse input messages! *)
          c :: [] =>
            if String.isPrefix {Prefix = "data:image", Full = c.ImageUrl.Url}
            then (case String.ssplit {Haystack = String.suffix c.ImageUrl.Url 5, Needle = ";base64,"} of
                Some (mimeType, imageData) => Success (UserImage {Image = WorldFfi.base64Decode imageData, MimeType = mimeType})
              | None => Failure <xml>ChatGPT: Bad base64 encoded image</xml>)
            else Success (UserImageUrl c.ImageUrl.Url)
        | _ => Failure <xml>Can only have one value per Chatgpt.message list!</xml>
      })
    (fn m => case m of
        PlainText x => {Role = x.Role, Content = make [#Str] x.Content}
      | UserImageUrl str => {Role = User, Content = make [#Lst] ({Typ = "image_url", ImageUrl = {Url = str}} :: [])}
      | UserImage img => {Role = User, Content = make [#Lst] ({Typ = "image_url", ImageUrl =
          {Url = "data:" ^ img.MimeType ^ ";base64," ^ WorldFfi.base64Encode img.Image}} :: [])}
    )
  end


datatype response_format = AsJson | RegularText

(* handling conversion between response_format and actual string *)
val read_response_format = mkRead' (fn s =>
  case s of
      "json_object" => Some AsJson
    | "text" => Some RegularText
    | _ => None)
  "response_format"

val show_response_format = mkShow (fn x =>
  case x of
      AsJson => "json_object"
    | RegularText => "text")

val json_response_format : json response_format = @json_derived Result.readResult show _

type model = string
val read_model = _
val show_model = _

functor Make(M : sig
                 val token : transaction (option string)
             end) = struct
    val token =
        tok <- M.token;
        case tok of
            Some tok => return tok
          | None => error <xml>How odd: no ChatGPT token!</xml>

    val urlPrefix = "https://api.openai.com/v1/"

    fun api url body =
        tok <- token;
        WorldFfi.post (bless (urlPrefix ^ url)) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) (Some "application/json") body

    structure Chat = struct
        type completionsArg = {Model : model, Messages : list message, ResponseFormat: {Type : response_format}, Temperature : float}
        val _ : json completionsArg =
          let val _ : json {Type : response_format} = json_record {Type = "type"}
          in json_record {
              Model = "model",
              Messages = "messages",
              ResponseFormat = "response_format",
              Temperature = "temperature"
            }
          end
        type response = {Choices : list {Message : {Role : role, Content : string}}}
        val _ : json response =
          let val _ : json {Role : role, Content : string} =
                json_record {Role = "role", Content = "content"}
              val _ : json {Message : {Role : role, Content : string}} =
                json_record {Message = "message"}
          in json_record {Choices = "choices"}
          end

        fun completions (arg : completionsArg) : transaction string =
            r <- api "chat/completions" (toJson arg);
            case (fromJson r : response).Choices of
                {Message = {Content = choice, ...}, ...} :: [] =>
                return choice
              | _ => error <xml>Unexpected number of choices in ChatGPT response</xml>
    end
end