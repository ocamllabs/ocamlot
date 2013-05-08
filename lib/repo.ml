type endpoint = Github of Github_hook.endpoint
type git = Uri.t
type 'a t = {
  url : Uri.t;
  repo_url : 'a;
 (* endpoint : endpoint; *)
}
type sha = string
type reference = Ref of string | Commit of string * sha
type 'a branch = {
  repo : 'a t;
  name : reference;
  label : string;
}

