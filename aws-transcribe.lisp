;;; Copyright (c) 2018 William R. Felts III, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;; aws-transcribe.lisp

(in-package #:aws-transcribe)

;;;
;;; if max-speaker-labels isn't nil, then
;;; the "ShowSpeakerLabels" willl automatically
;;; be set to "T".
(defstruct transcription-settings
  (max-speaker-labels nil)
  (vocabulary-name nil))

(defun settings-list (settings)
  `(
    ,@(if (transcription-settings-max-speaker-labels settings) `(("MaxSpeakerLabels" . ,(transcription-settings-max-speaker-labels settings))
								 ("ShowSpeakerLabels" . ,(xjson:json-bool t))))
      ,@(if (transcription-settings-vocabulary-name settings) `(("VocabularyName" . ,(transcription-settings-vocabulary-name settings))))))

(defun start-transcription (job-name url access-key secret-key &key (language-code "en-US") (media "wav") (sample-rate 16000) (settings nil))
  (assert (member language-code '("en-US" "es-US") :test #'equal))
  (assert (member media '("mp3" "mp4" "wav" "flac") :test #'equal))
  (assert (<= 8000 sample-rate 48000))
  (let ((settings-list (and settings (settings-list settings))))
    (aws-foundation:aws4-post "transcribe" "us-east-1"
			      "Transcribe.StartTranscriptionJob"
			      `(("LanguageCode" . ,language-code)
				("Media" . (("MediaFileUri" . ,url)))
				("MediaFormat" . ,media)
				("MediaSampleRateHertz" . ,sample-rate)
				("TranscriptionJobName" . , job-name)
			      ,@(if settings-list `(("Settings" . ,settings-list))))
			    :access-key access-key
			    :secret-key secret-key)))
  
(defun get-transcription (job-name access-key secret-key)
  (aws-foundation:aws4-post "transcribe" "us-east-1"
			    "Transcribe.GetTranscriptionJob"
			    `(("TranscriptionJobName" . , job-name))
			    :access-key access-key
			    :secret-key secret-key))

(defun list-transcription-jobs (access-key secret-key)
  (aws-foundation:aws4-post "transcribe" "us-east-1"
			    "Transcribe.ListTranscriptionJobs"
			    nil
			    :access-key access-key
			    :secret-key secret-key))

(defun get-transcription-job (result)
  (xjson:json-key-value :*TRANSCRIPTION-JOB result))

(defun get-transcription-job-status (transcription-job)
  (xjson:json-key-value :*TRANSCRIPTION-JOB-STATUS transcription-job))

(defun get-transcription-transcript (transcription-job)
  (xjson:json-key-value :*TRANSCRIPT transcription-job))

(defun get-transcript-file-uri (transcript)
  (xjson:json-key-value :*TRANSCRIPT-FILE-URI transcript))

(defun get-transcription-file-uri (result)
  (let* ((transcription-job (get-transcription-job result))
	 (transcription-job-status (get-transcription-job-status transcription-job)))
    (when (equal "COMPLETED" transcription-job-status)
      (get-transcript-file-uri (get-transcription-transcript transcription-job)))))
  
(defun wait-transcription (job-name access-key secret-key &optional (delay-seconds 5))
  (multiple-value-bind (result code response)
      (get-transcription job-name access-key secret-key)
    (if (and (equal code 200)
	     (equal "IN_PROGRESS" (get-transcription-job-status (get-transcription-job result))))
	(progn
	  (sleep delay-seconds)
	  (wait-transcription job-name access-key secret-key delay-seconds))
	(values result code response))))

(defun get-transcription-file (transcription-file-uri)
  (aws-foundation:aws-get transcription-file-uri))

(defun transcription-result (transcription-file)
  (values (xjson:json-key-value :TRANSCRIPT (first (xjson:json-key-value :TRANSCRIPTS (xjson:json-key-value :RESULTS transcription-file))))
	  (xjson:json-key-value :STATUS  transcription-file)))
