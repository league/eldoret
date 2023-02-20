{
  description = "An Emacs configuration.";

  inputs.emacs-overlay = {
    url = "github:nix-community/emacs-overlay";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  # My own fork of notmuch mail indexer, with more sorting options.
  inputs.notmuch-sort-fork = {
    url = "github:league/notmuch/sort";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, emacs-overlay, notmuch-sort-fork, ... }:
    let
      inherit (nixpkgs) lib;

      # Generate attribute set for each supported system.  The ‘mk’ argument is
      # a function that is given a package set with overlays already applied.
      eachSystem = mk:
        lib.genAttrs [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ]
        (system:
          mk (import nixpkgs {
            inherit system;
            overlays = [ self.overlays.default ];
          }));

      # It’s helpful for emacs packages to set the ‘mainProgram’ attribute.
      setMainProgram = d:
        d.overrideAttrs
        (old: { meta = old.meta // { mainProgram = "emacs"; }; });

      # Collect the names of custom emacs variants defined in the overlay, and
      # define an iterator to construct attribute sets.
      emacsAttrs = lib.attrNames (self.overlays.emacsen { } { });
      eachEmacs = mk: lib.listToAttrs (builtins.map mk emacsAttrs);

      patchExePath = var: def: path: file: ''
        sed -i 's,\(${var}\) "${def}",\1 "${path}",' "${file}"
      '';
    in {
      overlays.lib = _final: _prev: { inherit lib; };

      # This overlay tweaks the packageBuild tool so that all packages built
      # with nix will include README files alongside the code.  Otherwise, we
      # have only Info files and Commentary sections in Elisp files.
      overlays.packageBuild = final: prev: {
        emacsPackagesFor = emacs:
          (prev.emacsPackagesFor emacs).overrideScope' (eself: esuper: {
            melpaBuild = args:
              (esuper.melpaBuild args).overrideAttrs (before: {
                packageBuild = before.packageBuild.overrideAttrs (old: {
                  patches = [ ./package-build-with-readme.patch ]
                    ++ old.patches;
                });
              });

            # These show how we can embed certain executable command paths into
            # the elisp files, to make sure they are part of the closure.
            notmuch = esuper.notmuch.overrideAttrs (_: {
              postPatch = patchExePath "notmuch-command" "notmuch"
                "${final.notmuch}/bin/notmuch" "emacs/notmuch-lib.el";
            });

            # Embed direnv command path.
            envrc = esuper.envrc.overrideAttrs (_: {
              postPatch = patchExePath "envrc-direnv-executable" "direnv"
                "${final.direnv}/bin/direnv" "envrc.el";
            });

            # Ledger recipe ends up omitting info file.
            ledger-mode = eself.melpaBuild {
              inherit (esuper.ledger-mode) pname version commit src;
              recipe = final.writeText "ledger-mode" ''
                (ledger-mode :fetcher github :repo "ledger/ledger-mode"
                  :files ("ledger*.el" "doc/ledger-mode.texi" "README*")
                  :old-names (ldg-mode))
              '';
              postPatch = patchExePath "ledger-binary-path" "ledger"
                "${final.ledger}/bin/ledger" "ledger-exec.el";
            };
          });
      };

      # Emacs variants are defined using overlays.  They can vary in build
      # arguments, package selections, etc.
      overlays.emacsen = let
        ttyPkgs = p: [
          p.auto-compile # Automatically (re-)compile elisp code
          p.avy # Jump to arbitrary positions in visible text
          p.cyphejor # Abbreviate major-mode names
          p.diff-hl # Highlight uncommitted changes using VC
          p.diminish # Remove or abbreviate minor-mode indicators
          p.dockerfile-mode # Major mode for editing Dockerfiles
          p.echo-bar # Turn echo area into a custom status bar
          p.ef-themes # Colorful and legible themes
          p.elm-mode # Major mode for Elm language
          p.envrc # Load environment based on .envrc in working dir
          p.evil-collection # Further keybindings and tools for Evil
          p.evil-commentary # Evil operators for (un-)commenting
          p.evil-escape # Escape from anything with a customizable key sequence
          p.evil # I guess I joined the Dark Side™
          p.evil-numbers # Increment/decrement numbers near point
          p.evil-surround # Add/remove/change paired delimiters around point
          p.evil-terminal-cursor-changer # Cursor shape & color in terminal
          p.evil-textobj-line # Text object representing current line
          p.general # Convenient macros for keybindings
          p.haskell-mode # Edit Haskell programs
          p.hl-todo # Highlight “TO-DO” and similar keywords
          p.magit # A git porcelain inside Emacs
          p.markdown-mode # Major mode for Markdown-formatted text
          p.modus-themes # Elegant, highly legible and customizable themes
          p.nix-mode # Language mode for nix expressions
          p.no-littering # Keep ‘user-emacs-directory’ clean
          p.olivetti # Centered, constrained-width editing
          p.orderless # Complete by matching multiple regexps in any order
          p.outshine # Org-inspired outline minor mode
          p.php-mode # Major mode for PHP language
          p.rainbow-mode # Colorize color specs like #bff
          p.racket-mode
          p.telephone-line # A pretty and configurable mode line
          p.typescript-mode # Major mode for Typescript language
          p.undo-tree # Treat undo history as a tree
          p.use-package # Configuration macros
          p.vertico # Vertical interactive completion
          p.which-key # Display available keybindings in a pop-up
          p.yaml-mode # Major mode for editing YAML files
        ];
        guiPkgs = p:
          ttyPkgs p ++ [
            p.benchmark-init # Record times for ‘require’ and ‘load’ calls
            p.default-text-scale # Adjust font size in all frames
            p.evil-ledger # Make ledger-mode more evil
            p.fontaine # Set font configurations using presets
            p.ledger-mode # Manage financial accounts
          ];
        mailPkgs = p:
          guiPkgs p ++ [
            p.notmuch # Run notmuch email indexer within emacs
            p.ol-notmuch # Org links to notmuch messages and threads
          ];
      in _final: prev: {
        lemacs-nox = setMainProgram (prev.emacs-nox.pkgs.withPackages ttyPkgs);
        lemacs = setMainProgram (prev.emacs.pkgs.withPackages guiPkgs);
        lemacs-mail = setMainProgram (prev.emacs.pkgs.withPackages mailPkgs);
      };

      overlays.default = lib.composeManyExtensions [
        # Ordering of overlays is important! We need notmuch-sort-fork
        # to come AFTER emacs-overlay.
        self.overlays.lib
        emacs-overlay.overlays.package
        emacs-overlay.overlays.emacs
        notmuch-sort-fork.overlays.default
        self.overlays.emacsen
        self.overlays.packageBuild
      ];

      packages = eachSystem (pkgs:
        eachEmacs (name: {
          # The emacs derivation with bundled packages.
          inherit name;
          value = pkgs.${name};
        }) // eachEmacs (name: {
          # Derivation for the byte-compiled init file.  It is used only for
          # running the configuration as a stand-alone app with “nix run”, but
          # the package is also exported here for dev convenience.  The ‘egrep’
          # is because ‘batch-byte-compile’ doesn’t seem to reliably set an exit
          # status on error.  We also set ‘byte-compile-error-on-warn’ in the
          # init file itself, so warnings are flagged as errors.  Unfortunately,
          # byte- compiling needs a writable ‘$XDG_CONFIG_HOME’ because loading
          # ‘no-littering’ calls ‘make-directory’ for the etc/ and var/ subdirs.
          name = "${name}-init-elc";
          value =
            pkgs.runCommand "init.elc" { buildInputs = [ pkgs.${name} ]; } ''
              set -v
              cp ${./.}/init.el ./init.el
              export XDG_CONFIG_HOME="$PWD/home"
              mkdir -vp "$XDG_CONFIG_HOME/emacs"
              emacs --batch -f batch-byte-compile init.el |& tee ./init.log
              egrep --quiet '^(Error|Cannot)' ./init.log && false
              cp ./init.elc "$out"
            '';
        }));

      apps = eachSystem (pkgs:
        eachEmacs (name: {
          inherit name;
          value.type = "app";
          # I decided to substantially simplify how this (supposedly) stand-
          # alone run works.  Setting either $HOME or $XDG_CONFIG_HOME to /tmp
          # are problematic for routine use.  So assuming chemacs is active, we
          # can set ‘user-emacs-directory’ to PWD and invoke the appropriate
          # emacs-with-packages.
          value.program = toString (pkgs.writeShellScript "emacs-standalone" ''
            set -v
            ${pkgs.${name}}/bin/emacs --with-profile \
              "((user-emacs-directory . \"$PWD\"))" "$@"
          '');
        }));
    };
}
