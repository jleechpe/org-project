;;; org-project.el --- Create deadline templates for Org

;; Copyright (C) 2013  Jonathan Leech-Pepin

;; Author: Jonathan Leech-Pepin <jonathan@leechpepin.com>
;; Keywords: outlines, convenience

;; This program is free software; you can redistribute it and/or modify
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

;; This code allows for the creation of bulk subtasks in Org-mode as
;; defined in the customizable variable `op-subtask-series'.  This is
;; useful for projects where the deadlines are predefined for any
;; given project.
;;
;; Project insertion is called with `op-create-project', which
;; prompts for the project name, the category (for use in Agenda
;; views), and the final due date.  All other dates are calculated
;; using the offset in `op-subtask-series'.
;;
;; With the default options, the project (top headline) will receive a
;; TODO state as well as the subtasks.  Both project and subtask TODOs
;; can be customized with `op-master-todo' and `op-default-todo'
;; respectively.
;;
;; By default, the inserted dates are created as deadlines, this can
;; be changed to scheduled dates.
;;
;; By default, weekends are ommited when calculating the offset dates.

;;; Code:

;; Requires org
(require 'org)
(require 'org-element)

;; Needs cl for loop
(eval-when-compile
  (require 'cl))

;;; User variables

(defgroup org-project 'nil
  "Creation of project timelines from an initial date.")

(defcustom op-subtask-series
  '(("Hand-off" 1)
    ("Review start" 14)
    ("Review end" 7)
    ("Localization" 21)
    ("Start research" 30))
  "Create a list of entries for the various subtasks to have
completion dates.

Each entry is a list containing a string with the desired name of
the task, and an integer for the number of days preceeding the
final deadline for the entry to be given a deadline for.

Negative integers will set the date in the future, while positive
will set it in the past."
  :type '(repeat :tag "Set of subtasks to be included"
                 (list (string :tag "Name of subtask")
                       (integer :tag "Days offset")))
  :group 'org-project)

(defcustom op-master-todo 't
  "Set parent headline with same TODO state as subtasks?

If set to nil, no TODO state will be given to the parent
headline.

If set to a string, use that string as the TODO state."
  :type '(choice
          (boolean :tag "Yes" 't)
          (boolean :tag "No" 'nil)
          (string :tag "Custom state"))
  :group 'org-project)

(defcustom op-default-todo "TODO"
  "Default TODO state for project."
  :type 'string
  :group 'org-project)

(defcustom op-planning-deadline 't
  "Default to setting as deadline.

If nil use scheduled instead"
  :group 'org-project
  :type '(choice
          (boolean :tag "Deadline" 't)
          (boolean :tag "Scheduled" 'nil)))

(defcustom op-allow-weekend 'nil
  "Allow weekend dates in created projects.

Defaults to nil."
  :group 'org-project
  :type '(choice
          (boolean :tag "No" 'nil)
          (boolean :tag "Yes" 't)))

;;; Internal functions

(defun op-create-timestamp (time &optional weekend)
  "Create an org timestamp for TIME.

Do not insert it, simply provide it as a string.
If WEEKEND is non-nil, allow dates on weekends."
  (let ((weekend (or weekend
                     op-allow-weekend)))
    (unless weekend
      (let ((dow (string-to-int
                  (format-time-string "%u" time))))
        (if (< 5  dow)
            (setq time
                  (org-read-date 'nil 't
                                 (format "-%sd" (- 5 dow))
                                 'nil time)))))
    (format-time-string (car org-time-stamp-formats) time)))

(defun op-create-headline (data)
  "Use the p-list contained in DATA to create an Org headline.

Headlines only include title, level, schedule or deadline dates
and todo state."
  (let* ((title    (plist-get data :title))
         (level    (plist-get data :level))
         (sched    (plist-get data :schedule))
         (deadl    (plist-get data :deadline))
         (todo     (plist-get data :todo))
         (headline `(headline (:level ,level
                               :title ,title
                               :todo-keyword ,todo)))
         (planning `(planning (:deadline ,(if deadl
                                              `(timestamp
                                                (:raw-value
                                                 ,(op-create-timestamp
                                                   deadl)))
                                            nil)
                               :scheduled ,(if sched
                                               `(timestamp
                                                 (:raw-value
                                                  ,(op-create-timestamp
                                                    sched))))))))
    (org-element-adopt-elements headline planning)))

(defun op-create-project (name category initial &optional level todo)
  "NAME is the top level headline for the project and INITIAL is the due date.

CATEGORY will provide an agenda category to clarify which project
the subtasks belong to.  If none is set, the project NAME will be
used instead.

This will create an entire subtree of associated tasks as defined
in `op-subtask-series'."
  (interactive
   (list
    (read-string "Project Name: ")
    (read-string "Agenda Category (leave blank for project name): ")
    (org-read-date 'nil 'nil 'nil "Due date: ")))
  (let* ((current  (save-excursion
                     (ignore-errors (org-back-to-heading))
                     (org-element-at-point)))
         (endpt    (org-element-property :end current))
         (clevel   (org-element-property :level current))
         (todo     (or todo op-default-todo))
         (master   (cond
                    ((stringp op-master-todo)
                     op-master-todo)
                    ('t todo)
                    ('nil 'nil)))
         (initial  (org-read-date 'nil 't initial))
         (level    (or level clevel))
         (schedule (if op-planning-deadline 'nil initial))
         (deadline (if op-planning-deadline initial 'nil))
         (category (or category name))
         (project  `(:title ,name
                     :level ,level
                     :schedule ,schedule
                     :deadline ,deadline
                     :todo ,master))
         (cat      `(property-drawer ()
                                     (node-property
                                      (:key "CATEGORY"
                                       :value ,category))))
         (toplevel (op-create-headline project))
         (subproj  (loop for entry in op-subtask-series collect
                         (let* ((level    (+ 1 level))
                                (offset   (- 0 (cadr entry)))
                                (actual   (org-read-date
                                           'nil 't
                                           (format "-%dd" offset) 'nil initial))
                                (schedule (if op-planning-deadline 'nil actual))
                                (deadline (if op-planning-deadline actual 'nil))
                                (task     `(:title ,(car entry)
                                            :level ,level
                                            :schedule ,schedule
                                            :deadline ,deadline
                                            :todo ,todo)))
                           (op-create-headline task)))))
    (org-save-outline-visibility 't
      (goto-char endpt)
      (insert (org-element-interpret-data
               (apply 'org-element-adopt-elements toplevel cat subproj))))))

(provide 'org-project)
;;; org-project.el ends here
