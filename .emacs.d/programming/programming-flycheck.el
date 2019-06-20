;;; package -- Summary
;; Flycheck config

;;; Commentary:

(use-package flycheck
  :commands flycheck-mode
  :diminish " Ⓕ"
  :defer t
  :config
  (progn

    ;; color mode line faces
    (defun monkey/defface-flycheck-mode-line-color (state)
      "Define a face for the given Flycheck STATE."
      (let* ((fname (intern (format "monkey-mode-line-flycheck-%s-face"
                                    (symbol-name state))))
             (foreground (face-foreground
                          (intern (format "flycheck-fringe-%s" state)))))
        (eval `(defface ,fname '((t ()))
                 ,(format "Color for Flycheck %s feedback in mode line."
                          (symbol-name state))
                 :group 'monkey))
        (set-face-attribute fname nil
                            :foreground foreground
                            :box (face-attribute 'mode-line :box))))

    (defun monkey/set-flycheck-mode-line-faces ()
      "Define or set the flycheck info mode-line faces."
      (mapcar 'monkey/defface-flycheck-mode-line-color
              '(error warning info)))
    (monkey/set-flycheck-mode-line-faces)

    (defmacro monkey/custom-flycheck-lighter (error)
      "Return a formatted string for the given ERROR (error, warning, info)."
      `(let* ((error-counts (flycheck-count-errors
                             flycheck-current-errors))
              (errorp (flycheck-has-current-errors-p ',error))
              (err (or (cdr (assq ',error error-counts)) "?"))
              (running (eq 'running flycheck-last-status-change)))
         (if (or errorp running) (format "%s " err))))

    ;; Custom fringe indicator
    (when (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'my-flycheck-fringe-indicator
        (vector #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00011100
                #b00111110
                #b00111110
                #b00111110
                #b00011100
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b01111111)))

    (flycheck-define-error-level 'error
                                 :overlay-category 'flycheck-error-overlay
                                 :fringe-bitmap 'my-flycheck-fringe-indicator
                                 :fringe-face 'flycheck-fringe-error)

    (flycheck-define-error-level 'warning
                                 :overlay-category 'flycheck-warning-overlay
                                 :fringe-bitmap 'my-flycheck-fringe-indicator
                                 :fringe-face 'flycheck-fringe-warning)

    (flycheck-define-error-level 'info
                                 :overlay-category 'flycheck-info-overlay
                                 :fringe-bitmap 'my-flycheck-fringe-indicator
                                 :fringe-face 'flycheck-fringe-info)

    (add-hook 'after-init-hook #'global-flycheck-mode)))

(use-package flycheck-pos-tip
  :init (eval-after-load 'flycheck
          '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(with-eval-after-load "helm"
  (use-package helm-flycheck
    :bind (("C-c ! !" . helm-flycheck))))

(provide 'programming-flycheck)
;;; programming-flycheck.el ends here
