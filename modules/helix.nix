{ ... }:

{
  programs.helix = {
    enable = true;
    settings = {
      theme = "onelight";

      editor.line-number = "relative";

      editor.cursor-shape = {
        insert = "bar";
        normal = "block";
        select = "underline";
      };
    };
  };
}
