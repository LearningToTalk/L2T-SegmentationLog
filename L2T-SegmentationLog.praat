# A procedure that functions as a constant for the column numbers and column
# names of the various columns in a SegmentationLog.
procedure segmentation_log_columns
  .segmenter         = 1
  .segmenter$        = "Segmenter"
  .start_time        = 2
  .start_time$       = "StartDate"
  .end_time          = 3
  .end_time$         = "EndDate"
  .trials            = 4
  .trials$           = "NumberOfTrials"
  .segmented_trials  = 5
  .segmented_trials$ = "NumberOfTrialsSegmented"
  # Gather the column names into a vector.
  .slot1$ = .segmenter$
  .slot2$ = .start_time$
  .slot3$ = .end_time$
  .slot4$ = .trials$
  .slot5$ = .segmented_trials$
  .length = 5
  .all_columns$ = .slot1$
  for i from 2 to .length
    .all_columns$ = .all_columns$ + " " + .slot'i'$
  endfor
endproc



procedure segmenters_initials: .segment_log_filepath$
  @parse_filepath: .segment_log_filepath$
  .initials$ = mid$(parse_filepath.filename$,
                ... rindex(parse_filepath.filename$, "_") + 1,
                ... 2)
endproc




procedure segmentation_log_error: .directory$ 
                              ... .participant_number$
  printline
  printline
  printline <<<>>> <<<>>> <<<>>> <<<>>> <<<>>> <<<>>> <<<>>> <<<>>> <<<>>>
  printline
  printline ERROR :: No Segmentation Log was loaded
  printline
  printline Make sure the following directory exists on your computer:
  printline '.directory$'
  printline 
  printline Also, make sure that directory contains a Segmentation Log
        ... file for participant '.participant_number$'.
  printline
  printline <<<>>> <<<>>> <<<>>> <<<>>> <<<>>> <<<>>> <<<>>> <<<>>> <<<>>>
  printline
  printline 
endproc


# A procedure for determining the checking initials from a filepath and the
# checker's initials.
procedure checking_initials: .filepath$, .checkers_initials$
  @segmenters_initials: .filepath$
  .initials$ = segmenters_initials.initials$ + .checkers_initials$
endproc


# A procedure for determining how the length of the initials sequence in
# a filename.
# Note: .filename$ must end in segmentLog.txt
procedure initials_sequence: .filename$
  .length = length(.filename$)
  .left_index = rindex(.filename$, "_") + 1
  .suffix$ = mid$(.filename$, .left_index, .length - .left_index + 1)
  .initials$ = replace$(.suffix$, "segmentLog.txt", "", 1)
  .n_char = length(.initials$)
endproc


procedure read_segmentation_log
  if session_parameters.activity$ == praat_activities.check$
    # Read in the Segmentation Log from the filesystem.
    Read Table from tab-separated file... 'segmentation_log.read_from$'
    # Rename the Segmentation Log Table object.
    @participant: segmentation_log.read_from$, 
              ... session_parameters.participant_number$
    .table_obj$ = participant.id$ + "_Log" + checking_initials.initials$
    Rename... '.table_obj$'
    # Store the name of the Segmentation Log Table object.
    .praat_obj$ = selected$()
  endif
endproc


procedure segmentation_log
  # Import constants from [@session_parameters] namespace.
  # Note: As currently written, [segmentation_log] assumes that 
  #       @session_parameters has been called prior to the @segmentation_log 
  #       call.  If you want to generalize [segmentation_log] so that this 
  #       assumption is not made, then all of the constants in the following 
  #       block of code need to be made arguments to the [segmentation_log] 
  #       function.
  .activity$             = session_parameters.activity$
  .experiment_directory$ = session_parameters.experiment_directory$
  .initials$             = session_parameters.initials$
  .experimental_task$    = session_parameters.experimental_task$
  .participant_number$   = session_parameters.participant_number$
  # Set up the [segmentation_log_columns] namespace.
  @segmentation_log_columns
  # Only do something if the [.experiment_directory$] is set up.
  if .experiment_directory$ <> ""
    # The function of the procedure depends on the [.activity$].
    if .activity$ == praat_activities.segment$
  
    # If the current user is checking a Segmented TextGrid.
    elif .activity$ == praat_activities.check$
      # Set the [.row_on_segmentation_log] that is used when checking a 
      # Segmented TextGrid.
      .row_on_segmentation_log = 2
      # Set up the path to the [.directory$] of Segmentation Logs.
      .directory$ = .experiment_directory$ + "/" +
                ... "Segmentation" + "/" +
                ... "ToCheck"
      # First use a pattern that tries to match XXYYsegmentLog.txt, where
      # XX are the initials of first segmenter and YY are the initials of the
      # segmentation-checker.
      .pattern$ = .directory$ + "/" +
              ... .experimental_task$ + "_" +
              ... .participant_number$ + "*" +
              ... .initials$ + "segmentLog.txt"
      @filename_from_pattern: .pattern$, "Segmentation Log"
      if filename_from_pattern.filename$ <> ""
        # If the segmentation-checker is the same person as the segmenter, then
        # this method for finding the segmentation log will also match the
        # original segmentation log.  E.g., it will match "...RCsegmentLog.txt"
        # and "...RCRCsegmentLog.txt".  So, check the length of the initials
        # sequence.
        @initials_sequence: filename_from_pattern.filename$
        if initials_sequence.n_char == 2
          .continuing_previous_session = 0
          .read_from$ = .directory$ + "/" + filename_from_pattern.filename$
          @segmenters_initials: .read_from$
          @checking_initials: .read_from$, .initials$
          .write_to$ = replace$(.read_from$, segmenters_initials.initials$,
                            ... checking_initials.initials$, 1)
        elif initials_sequence.n_char == 4
          .continuing_previous_session = 1
          # Set up the path that the Segmentation Log is [.read_from$]
          .read_from$ = .directory$ + "/" + filename_from_pattern.filename$
          # Set up the [.write_to$] path.
          .write_to$  = .read_from$
          # Determine the [checking_initials].
          @checking_initials: .read_from$, .initials$
        endif
      else
        .continuing_previous_session = 0
        # If no match to XXYYsegmentLog.txt was found, then use a pattern that
        # matches just the [.participant_number$].
        .pattern$ = .directory$ + "/" +
                ... .experimental_task$ + "_" +
                ... .participant_number$ + "*" +
                ... "segmentLog.txt"
        @filename_from_pattern: .pattern$, "Segmentation Log"
        if filename_from_pattern.filename$ <> ""
          .read_from$ = .directory$ + "/" + filename_from_pattern.filename$
          # Parse the [.segmenters_initials$] from the [.read_from$] path and
          # use them to set up the [.write_to$] path.
          @segmenters_initials: .read_from$
          @checking_initials: .read_from$, .initials$
          .write_to$  = replace$(.read_from$, segmenters_initials.initials$,
                             ... checking_initials.initials$, 1)
        else
          # No Segmentation Log could be found; print an error.
          .read_from$ = ""
          .write_to$  = ""
          .praat_obj$ = ""
        endif
      endif
      # If the [.read_from$] path has been set up...
      if .read_from$ <> ""
        # Read in the Segmentation Log.
        @read_segmentation_log
        # Import the name of the [.praat_obj$].
        .praat_obj$ = read_segmentation_log.praat_obj$
        # If the user is checking a segmented TextGrid, then check if this is
        # her first session by checking the number of rows on Segmentation Log
        # Table.
        select '.praat_obj$'
        .n_rows = Get number of rows
        if .n_rows == 1
          # Add a row and populate it.
          select '.praat_obj$'
          Append row
          # The initials of the checker.
          Set string value: .row_on_segmentation_log,
                        ... segmentation_log_columns.segmenter$,
                        ... .initials$
          # The start time.
          @timestamp
          Set string value: .row_on_segmentation_log,
                        ... segmentation_log_columns.start_time$,
                        ... timestamp.time$
          # The end time.
          @timestamp
          Set string value: .row_on_segmentation_log,
                        ... segmentation_log_columns.end_time$,
                        ... timestamp.time$
          # The number of trials.
          .n_trials = Get value... 1 'segmentation_log_columns.trials$'
          Set numeric value: .row_on_segmentation_log,
                         ... segmentation_log_columns.trials$,
                         ... .n_trials
          # The number of trials segmented (= 0).
          Set numeric value: .row_on_segmentation_log,
                         ... segmentation_log_columns.segmented_trials$,
                         ... 0
        else
          # Just update the EndDate column.
          @timestamp
          select '.praat_obj$'
          Set string value: .row_on_segmentation_log,
                        ... segmentation_log_columns.end_time$,
                        ... timestamp.time$
        endif
      endif
    endif
    
  else
    # If the [.experiment_directory$] is not set up, then set all of the 
    # string constants to empty strings.
    .read_from$ = ""
    .write_to$  = ""
    .praat_obj$ = ""
    # Print a message to let the user know that no Segmentation Log was loaded.
    printline No Segmentation Log was loaded because the current workstation
          ... is not recognized.
  endif
endproc


