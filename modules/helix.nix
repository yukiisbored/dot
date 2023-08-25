{ ... }:

{
  programs.helix = {
    enable = true;
    settings = {
      theme = "ayu_evolve";

      editor.line-number = "relative";

      editor.cursor-shape = {
        insert = "bar";
        normal = "block";
        select = "underline";
      };

      editor.whitespace.characters = {
        space = "·";
        nbsp = "⍽";
        tab = "→";
        newline = "⏎";
        tabpad = "·" ;
      };

      editor.indent-guides = {
        render = true;
        character = "▏";
        skip-levels = 1;
      };
    };
  };
}
