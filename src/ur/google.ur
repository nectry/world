open Json

signature S = sig
    val client_id : string
    val client_secret : string
    val https : bool
end

type profile = { ResourceName : string }

val json_profile : json profile =
    json_record {ResourceName = "resourceName"}

structure OauthP = struct
    val authorize_url = bless "https://accounts.google.com/o/oauth2/auth"
    val access_token_url = bless "https://oauth2.googleapis.com/token"
end
    
functor Login(M : S) = struct
    open M

    table secrets : { ResourceName : string,
                      Secret : int }
      PRIMARY KEY ResourceName
                    
    cookie user : { ResourceName : string, Secret : int }

    fun withToken tok =
        profile <- WorldFfi.get (bless "https://people.googleapis.com/v1/people/me?personFields=emailAddresses") (Some ("Bearer " ^ tok));
        (profile : profile) <- return (Json.fromJson profile);
        secret <- oneOrNoRowsE1 (SELECT (secrets.Secret)
                                 FROM secrets
                                 WHERE secrets.ResourceName = {[profile.ResourceName]});
        secret <- (case secret of
                       Some secret => return secret
                     | None =>
                       secret <- rand;
                       dml (INSERT INTO secrets(ResourceName, Secret)
                            VALUES ({[profile.ResourceName]}, {[secret]}));
                       return secret);

        setCookie user {Value = {ResourceName = profile.ResourceName, Secret = secret},
                        Expires = None,
                        Secure = https}

    open Oauth.Make(struct
                        open M
                        open OauthP

                        val withToken = withToken
                        val scope = Some "profile"
                    end)

    val whoami =
        c <- getCookie user;
        case c of
            None => return None
          | Some r =>
            ok <- oneRowE1 (SELECT COUNT( * ) > 0
                            FROM secrets
                            WHERE secrets.ResourceName = {[r.ResourceName]}
                              AND secrets.Secret = {[r.Secret]});
            if ok then
                return (Some r.ResourceName)
            else
                return None

    val logout = clearCookie user
end

type message_id = string
val show_message_id = _

type thread_id = string
val show_thread_id = _

type message = {
     Id : message_id,
     ThreadId : thread_id
}

type messages = {
     Messages : list message,
     ResultSizeEstimate : int
}

val _ : json message = json_record {Id = "id",
                                    ThreadId = "threadId"}
val _ : json messages = json_record {Messages = "messages",
                                     ResultSizeEstimate = "resultSizeEstimate"}
                       
functor Gmail(M : S) = struct
    open M

    table secrets : { Secret : int,
                      Token : string }
      PRIMARY KEY Secret
         
    cookie user : int
         
    fun withToken tok =
        secret <- rand;
        dml (INSERT INTO secrets(Secret, Token)
             VALUES ({[secret]}, {[tok]}));
        setCookie user {Value = secret,
                        Expires = None,
                        Secure = https}

    open Oauth.Make(struct
                        open M
                        open OauthP

                        val withToken = withToken
                        val scope = Some "https://www.googleapis.com/auth/gmail.readonly"
                    end)

    val logout = clearCookie user

    val token =
        c <- getCookie user;
        case c of
            None => error <xml>You must be logged into Google to use this feature.</xml>
          | Some n =>
            tokopt <- oneOrNoRowsE1 (SELECT (secrets.Token)
                                     FROM secrets
                                     WHERE secrets.Secret = {[n]});
            case tokopt of
                None => error <xml>You must be logged into Google to use this feature.</xml>
              | Some tok => return tok

    fun api url =
        tok <- token;
        WorldFfi.get url (Some ("Bearer " ^ tok))
        
    val messages =
        s <- api (bless "https://www.googleapis.com/gmail/v1/users/me/messages");
        return (fromJson s)
        (*api (bless "https://www.googleapis.com/gmail/v1/users/me/history?historyTypes=messageAdded&startHistoryId=0")*)
end