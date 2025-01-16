;;; aws.el --- Utilities to work with AWS.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Pavel Popov

;; Author: Pavel Popov <hotter-plazas-0x@icloud.com>
;; URL: https://github.com/velppa/VELLPA/tree/main/aws.el
;; Version: 0.1-pre
;; Package-Requires: ((emacs "29.1"))
;; Keywords: aws

;; This file is not part of GNU Emacs.

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

;; This package allows flanges to be easily frobnicated.

;;;; Installation

;;;;; Manual

;; Put this file in your load-path, and put this in your init file:

;; (require 'aws)

;;;; Usage

;; Run one of these commands:

;; `aws-browse': Browse Console for ARN.

;;; Code:

;;;; Requirements

(require 'rx)

;;;; Customization

;;;; Variables

;;;;; Keymaps

;;;; Commands

;;;###autoload
(cl-defun aws-browse (arn &key (region "eu-west-1") page)
  "Browse AWS console using provided ARN or resource ID.

Supported resources:
- vpc-XXX – VPC
- sg-XXX - Security Group
- pcx-XXX - Peering connection
- i-XXX - EC2 Instance
- subnet-XXX - Subnet
- arn:aws:iam::123456789012:role/role-name - IAM role
- arn:aws:iam::123456789012:policy/policy-name - IAM policy
"
  (interactive)
  (let* ((security-group-rx (rx (group bol "sg-" (one-or-more (not whitespace)))))
         (vpc-rx (rx (group bol "vpc-" (one-or-more (not whitespace)))))
         (instance-rx (rx (group bol "i-" (one-or-more (not whitespace)))))
         (peering-connection-rx (rx (group bol "pcx-" (one-or-more (not whitespace)))))
         (subnet-rx (rx (group bol "subnet-" (one-or-more (not whitespace)))))
         (iam-role-arn-rx (rx "arn:aws:iam::" (one-or-more digit) ":role/" (group (one-or-more (not whitespace)))))
         (iam-policy-arn-rx (rx "arn:aws:iam::" (one-or-more digit) ":policy/" (one-or-more (not whitespace))))
         (url (or (when (string-match security-group-rx arn)
                    (format "https://%s.console.aws.amazon.com/vpcconsole/home?region=%s#SecurityGroup:groupId=%s"
                            region region (match-string 1 arn)))
                  (when (string-match vpc-rx arn)
                    (format "https://%s.console.aws.amazon.com/vpcconsole/home?region=%s#VpcDetails:VpcId=%s"
                            region region (match-string 1 arn)))
                  (when (string-match iam-role-arn-rx arn)
                    (format "https://%s.console.aws.amazon.com/iam/home?region=%s#/roles/details/%s?section=permissions"
                            region region (match-string 1 arn)))
                  (when (string-match iam-policy-arn-rx arn)
                    (format "https://%s.console.aws.amazon.com/iam/home?region=%s#/policies/details/%s?section=permissions"
                            region region (url-hexify-string arn)))
                  (when (string-match peering-connection-rx arn)
                    (format "https://%s.console.aws.amazon.com/vpcconsole/home?region=%s#PeeringConnectionDetails:VpcPeeringConnectionId=%s"
                            region region (match-string 1 arn)))
                  (when (string-match instance-rx arn)
                    (format "https://%s.console.aws.amazon.com/ec2/home?region=%s#%s:instanceId=%s"
                            region region
                            (if (equal page :list) "Instances" "InstanceDetails")
                            (match-string 1 arn)))
                  (when (string-match subnet-rx arn)
                    (format "https://%s.console.aws.amazon.com/vpcconsole/home?region=%s#SubnetDetails:subnetId=%s"
                            region region (match-string 1 arn))))))
    (browse-url url)))

;;;; Functions

;;;;; Public

;;;;; Private

;;;; Footer

(provide 'aws)

;;; aws.el ends here
