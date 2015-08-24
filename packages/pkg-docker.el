(defgroup docker-tramp nil
  "TRAMP integration for Docker containers."
  :prefix "docker-tramp-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/emacs-pe/docker-tramp.el")
  :link '(emacs-commentary-link :tag "Commentary" "docker-tramp"))

(defcustom docker-tramp-docker-executable "docker"
  "Path to docker executable."
  :type 'string
  :group 'docker-tramp)

(defcustom docker-tramp-docker-options nil
  "List of docker options."
  :type '(repeat string)
  :group 'docker-tramp)

(defcustom docker-tramp-use-names nil
  "Whether use names instead of id."
  :type 'boolean
  :group 'docker-tramp)

;;;###tramp-autoload
(defconst docker-tramp-completion-function-alist
  '((docker-tramp--parse-running-containers  ""))
  "Default list of (FUNCTION FILE) pairs to be examined for docker method.")

;;;###tramp-autoload
(defconst docker-tramp-method "docker"
  "Method to connect docker containers.")


(defconst docker-tramp-method "docker"
  "Method to connect docker containers.")
(require-package 'docker-tramp)
(require 'docker-tramp)

(require-package 'dockerfile-mode)

(require-package 'docker)




(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))


(provide 'pkg-docker)
