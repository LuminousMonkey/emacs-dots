;;; Support drag and drop images into OrgMode
(use-package org-download
  :after org
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

(provide 'org-mode-download)
