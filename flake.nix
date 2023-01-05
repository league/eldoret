{
  description = "An Emacs configuration.";

  outputs = { self, nixpkgs, ... }:
    let
      inherit (nixpkgs) lib;

      # Generate attribute set for each supported system.  The ‘mk’ argument is
      # a function that is given a package set with overlays already applied.
      eachSystem = mk:
        lib.genAttrs [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ]
        (system:
          mk (import nixpkgs {
            inherit system;
            overlays = lib.attrValues self.overlays;
          }));

      # It’s helpful for emacs packages to set the ‘mainProgram’ attribute.
      setMainProgram = d:
        d.overrideAttrs
        (old: { meta = old.meta // { mainProgram = "emacs"; }; });

      # Collect the names of custom emacs variants defined in the overlay, and
      # define an iterator to construct attribute sets.  The ‘defaultEmacs’ must
      # be one of the ‘emacsAttrs’ names.  It is used in the devShell, and for
      # the ‘default’ package and app.
      emacsAttrs = lib.attrNames (self.overlays.default { } { });
      eachEmacs = mk: lib.listToAttrs (builtins.map mk emacsAttrs);
      defaultEmacs = "emacs";

      # Add a suffix to an attribute name to represent the compiled init file.
      mkInitAttr = name: name + "-init-elc";
      mkInitFilename = name: name + "-init.elc";
      mkTmpEmacsDir = name: "/tmp/dotemax-${name}/";

    in {
      # Emacs variants are defined using overlays.  They can vary in build
      # arguments, package selections, etc.
      overlays.default = let
        ttyPkgs = p: [ p.avy p.olivetti p.rainbow-mode p.use-package ];
        guiPkgs = p: ttyPkgs p ++ [ p.benchmark-init ];
      in _final: prev: {
        emacs-nox = setMainProgram (prev.emacs-nox.pkgs.withPackages ttyPkgs);
        emacs = setMainProgram (prev.emacs.pkgs.withPackages guiPkgs);
      };

      packages = eachSystem (pkgs:
        eachEmacs (name: {
          # The emacs derivation with bundled packages.
          inherit name;
          value = pkgs.${name};
        }) // eachEmacs (name: {
          # Derivation for the byte-compiled init file.  It is used only for
          # running the configuration as a stand-alone app with “nix run”, but
          # the package is exported here for dev convenience.  The ‘egrep’ is
          # because ‘batch-byte-compile’ doesn’t seem to reliably set an exit
          # status on error.  We also set ‘byte-compile-error-on-warn’ in the
          # init file itself, so warnings are flagged as errors.
          name = mkInitAttr name;
          value = pkgs.runCommand (mkInitFilename name) {
            buildInputs = [ pkgs.${name} ];
          } ''
            set -v
            cp ${./.}/init.el init.el
            emacs --batch -f batch-byte-compile init.el |& tee init.log
            egrep --quiet '^(Error|Cannot)' init.log && false
            cp init.elc "$out"
          '';
        }) // {
          default = pkgs.${defaultEmacs};
        });

      apps = eachSystem (pkgs:
        eachEmacs (name: {
          inherit name;
          value.type = "app";
          # To run emacs stand-alone, we use ‘--no-init-file’, but manually call
          # ‘package-activate-all’ to ensure that autoloads are available, and
          # then explicitly load the byte-compiled init file.  We also create a
          # placeholder ‘user-emacs-directory’ in ‘/tmp’ for state files and
          # customizations.
          value.program = toString (pkgs.writeShellScript "emacs-standalone" ''
            set -v
            mkdir -p "${mkTmpEmacsDir name}"
            ${pkgs.${name}}/bin/emacs --no-init-file \
              --eval '(setq user-emacs-directory "${mkTmpEmacsDir name}")' \
              --load ${./.}/early-init.el \
              --funcall package-activate-all \
              --load ${self.packages.${pkgs.system}.${mkInitAttr name}} "$@"
          '');
        }) // eachEmacs (name: {
          name = "${name}-chemacs";
          value.type = "app";
          value.program = toString (pkgs.writeShellScript "chemacs" ''
            set -v
            ${pkgs.${name}}/bin/emacs --with-profile \
              '((user-emacs-directory . "$PWD")
                (nix-elisp-bundle . "${pkgs.${name}.deps}"))'
          '');
        }) // {
          default = self.apps.${pkgs.system}.${defaultEmacs};
        });

      checks = eachSystem (pkgs:
        # Each variant should be able to byte-compile the init file.
        eachEmacs (name: {
          inherit name;
          value = self.packages.${pkgs.system}.${mkInitAttr name};
        }));

      devShells = eachSystem (pkgs: {
        default = pkgs.mkShell {
          buildInputs = [ pkgs.${defaultEmacs} pkgs.nixfmt pkgs.nix-linter ];
        };
      });
    };
}
