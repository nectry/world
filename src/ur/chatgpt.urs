functor TwoLegged(M : sig
                        val api_token : string
                    end) : sig
    val token : transaction (option string)
end

(** * API records *)

(* LLM models supported by ChatGPT *)
type model
val read_model : read model
val show_model : show model

datatype role = System | User | Assistant
val read_role : read role
val show_role : show role

(* Different response type we can ask the GPT to return *)
datatype response_format = AsJson | RegularText
val read_response_format : read response_format
val show_response_foramt : show response_format

(** * Now for the actual methods.... *)

functor Make(M : sig
                 val token : transaction (option string)
             end) : sig
    structure Chat : sig
        val completions : {Model : model,
                           Messages : list {Role : role,
                                            Content : string},
       (*The OpenAI documentation (found here: https://platform.openai.com/docs/api-reference/chat/create)
        dictates that response type is a field of a dict with key "type". 
        There are other values allowed in this dict, but we are only using "type" for now.*) 
                            ResponseFormat: {Type: response_format}}
                          -> transaction string
    end
end
