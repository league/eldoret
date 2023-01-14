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
      # define an iterator to construct attribute sets.  The ‘defaultEmacs’ must
      # be one of the ‘emacsAttrs’ names.  It is used in the devShell, and for
      # the ‘default’ package and app.
      emacsAttrs = lib.attrNames (self.overlays.emacsen { } { });
      eachEmacs = mk: lib.listToAttrs (builtins.map mk emacsAttrs);
      defaultEmacs = "lemacs";

    in {
      overlays.lib = _final: _prev: { inherit lib; };

      # This overlay tweaks the packageBuild tool so that all packages built
      # with nix will include README files alongside the code.  Otherwise, we
      # have only Info files and Commentary sections in Elisp files.
      overlays.packageBuild = _final: prev: {
        emacsPackagesFor = emacs:
          (prev.emacsPackagesFor emacs).overrideScope' (_: esuper: {
            melpaBuild = args:
              (esuper.melpaBuild args).overrideAttrs (before: {
                packageBuild = before.packageBuild.overrideAttrs (old: {
                  patches = [ ./package-build-with-readme.patch ]
                    ++ old.patches;
                });
              });
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
          p.general # Convenient macros for keybindings
          p.hl-todo # Highlight “TO-DO” and similar keywords
          p.magit # A git porcelain inside Emacs
          p.markdown-mode # Major mode for Markdown-formatted text
          p.modus-themes # Elegant, highly legible and customizable themes
          p.nix-mode # Language mode for nix expressions
          p.no-littering # Keep ‘user-emacs-directory’ clean
          p.olivetti # Centered, constrained-width editing
          p.php-mode # Major mode for PHP language
          p.rainbow-mode # Colorize color specs like #bff
          p.telephone-line # A pretty and configurable mode line
          p.typescript-mode # Major mode for Typescript language
          p.undo-tree # Treat undo history as a tree
          p.use-package # Configuration macros
          p.which-key # Display available keybindings in a pop-up
          p.yaml-mode # Major mode for editing YAML files
        ];
        guiPkgs = p:
          ttyPkgs p ++ [
            p.benchmark-init # Record times for ‘require’ and ‘load’ calls
            p.default-text-scale # Adjust font size in all frames
            p.fontaine # Set font configurations using presets
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
        }) // {
          default = pkgs.${defaultEmacs};
        });

      apps = eachSystem (pkgs:
        eachEmacs (name:
          let
            tmpHome = "/tmp/eldoret-${name}";
            initDir = "${tmpHome}/emacs";
            initElc = self.packages.${pkgs.system}."${name}-init-elc";
          in {
            inherit name;
            value.type = "app";
            # To run emacs stand-alone, we use ‘--no-init-file’, but manually call
            # ‘package-activate-all’ to ensure that autoloads are available, and
            # then explicitly load the byte-compiled init file.  We also create a
            # placeholder ‘user-emacs-directory’ in ‘/tmp’ for state files and
            # customizations.
            value.program = toString
              (pkgs.writeShellScript "emacs-standalone" ''
                if [ -f "$HOME/.emacs.el" -o \
                     -f "$HOME/.emacs" -o \
                     -d "$HOME/.emacs.d" ]; then
                    echo -n "Sorry, non-XDG emacs directories "
                    echo "interfere with a standalone run."
                else
                    set -v
                    mkdir -p "${initDir}"
                    ln -sf "${./.}/early-init.el" "${initDir}/early-init.el"
                    ln -sf "${./.}/init.el" "${initDir}/init.el"
                    ln -sf "${initElc}" "${initDir}/init.elc"
                    export XDG_CONFIG_HOME="${tmpHome}"
                    ${pkgs.${name}}/bin/emacs "$@"
                fi
              '');
          }) // {
            default = self.apps.${pkgs.system}.${defaultEmacs};
          });

      devShells = eachSystem (pkgs: {
        default = pkgs.mkShell {
          buildInputs = [ pkgs.${defaultEmacs} pkgs.nixfmt pkgs.nix-linter ];
          shellHook = ''
            echo
            emacs --batch -f batch-byte-compile init.el
            echo
          '';
        };
      });
    };
}
