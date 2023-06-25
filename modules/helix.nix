{ ... }:

{
  programs.helix = {
    enable = true;
    settings = {
      theme = "dark_high_contrast";

      editor.line-number = "relative";

      editor.cursor-shape = {
        insert = "bar";
        normal = "block";
        select = "underline";
      };
    };
  };
}
