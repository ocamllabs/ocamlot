type endpoint_type = Github of Github_hook.endpoint
type repo = {
  url : Uri.t;
  git_url : Uri.t;
  tasks : Task.t list;
  endpoint : endpoint_type;
}
