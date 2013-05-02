type endpoint = Github of Github_hook.endpoint
type git = Uri.t
type 'a t = {
  url : Uri.t;
  repo_url : 'a;
 (* endpoint : endpoint; *)
}
type 'a branch = { branch_repo : 'a t; branch_name : string; }

