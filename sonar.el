;;; sonar.el --- Provides sonar cloud integration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Damien Merenne <dam@cosinux.org>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'auth-source)
(require 'subr-x)
(require 'request)

(defcustom sonar-url "https://sonarcloud.io/api/"
  "The address of the sonar host."
  :type 'string
  :group 'sonar)

(defcustom sonar-debug nil "Debug the sonar calls." :type 'boolean :group 'sonar)

(defvar sonar-query-data '() "Query parameters as an alist.")

(defun sonar-log (level format-string &rest args)
  "Log message at LEVEL passing FORMAT-STRING and ARGS to `message'."
  (when sonar-debug (apply #'message format-string args)))

(defun sonar-debug (format-string &rest args)
  "Log debug message passing FORMAT-STRING and ARGS to `message'."
  (apply #'sonar-log :debug format-string args))

(defun sonar-info (format-string &rest args)
  "Log info message passing FORMAT-STRING and ARGS to `message'."
  (apply #'sonar-log :info format-string args))

(defun sonar-warn (format-string &rest args)
  "Log warn message passing FORMAT-STRING and ARGS to `message'."
  (apply #'sonar-log :warn format-string args))

(defun sonar-error (format-string &rest args)
  "Log error message passing FORMAT-STRING and ARGS to `message'."
  (apply #'sonar-log :error format-string args))

(defun sonar-credentials ()
  "Return username and password for sonar."
  (let ((found
         (nth 0
              (auth-source-search :max 1
                                  :host (url-host (url-generic-parse-url sonar-url))
                                  ;; secrets.el wouldn’t accept a number.
                                  :port (number-to-string (url-port (url-generic-parse-url sonar-url)))
                                  :require '(:user :secret)
                                  :create t)))
        user secret)
    (when found
      (setq user
            (plist-get found :user)
            secret
            (let ((sec (plist-get found :secret)))
              (if (functionp sec) (funcall sec) sec)))
      (list user secret))))

(defun sonar-authorization-token ()
  "Return authorization token for sonar."
  (destructuring-bind
      (username password)
      (sonar-credentials)
    `("Authorization" . ,(format "Basic %s" (base64-encode-string (concat password ":") t)))))

(defun sonar-url-query-string (data)
  "Convert alist DATA to url query parameters."
  (concat "?"
          (string-join
           (seq-map
            (lambda (item)
              (format "%s=%s"
                      (url-hexify-string (car item))
                      (url-hexify-string (cdr item))))
            data)
           "&")))

(defun sonar-request-url (&rest paths)
  "Build a sonar rest url form PATHS."
  (concat sonar-url
          (string-join paths "/")))

(defun sonar--parse ()
  "Parse response data."
  (let ((json-array-type 'list)) (json-read)))

(defun sonar-request (method paths &rest settings &allow-other-keys)
  "Send a METHOD rest request to PATHS."
  (let ((headers (list (sonar-authorization-token) '("Accept" . "application/json")))
        (sonar-url (apply #'sonar-request-url paths)))
    (sonar-debug "%s %s" method sonar-url)
    (apply #'request sonar-url :type method :headers headers :parser #'sonar--parse settings)))

(defun sonar-get (paths &rest settings &allow-other-keys)
  "Execute a get request to PATHS using request SETTINGS."
  (apply #'sonar-request "GET" paths settings))

(defun sonar-quality-gates-project-status (organization repo &optional branch &rest settings)
  "Fetch project status for ORGANIZATION and REPO."
  (let ((params
         `(("organization" . ,organization)
           ("projectKey" . ,(format "%s_%s" organization repo))
           ("branch" . ,(or branch "master")))))
    (apply #'sonar-get '("qualitygates" "project_status") :params params settings)))

(provide 'sonar)

;;; sonar.el ends here
