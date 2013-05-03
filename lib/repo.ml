type endpoint = Github of Github_hook.endpoint
type git = Uri.t
type 'a t = {
  url : Uri.t;
  repo_url : 'a;
 (* endpoint : endpoint; *)
}
type 'a branch = {
  repo : 'a t;
  name : string;
  label : string;
}

