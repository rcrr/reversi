;GIL-3
(
(
	:type                   :menu
	:name                   menu1
	:help                   ""
	:columns                1
	:menu-type              :command
	:menu-handler           nil
	:menu-title             ""
	:menu-item-labels       ("Preferences..." "Input..." "Load Game..." "Save Game..." "Show Moves" "Startposition" "Endposition" "Quit" )
	:menu-item-label-types  (:string :string :string :string :string :string :string :string )
	:menu-item-states       (:active :active :active :active :active :active :active :active )
	:menu-item-defaults     (nil nil nil nil nil nil nil nil )
	:initial-selections     (nil nil nil nil nil nil nil nil )
	:menu-item-handlers     (XCB_EinstellungenEin XCB_EingabeEin XCB_LADEN XCB_SPEICHERN XCB_Tabelle XCB_ZURUECK_ZUM XCB_VOR_ZUM XCB_ENDE )
	:menu-item-menus        (nil nil nil nil nil nil nil nil )
	:menu-item-colors       ("" "" "" "" "" "" "" "" )
	:pinnable               nil
	:user-data              ()
	:actions                (
		(
		:from                   (menu1 "Preferences...")
		:when                   (Notify )
		:to                     (menu1 "Preferences...")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_EinstellungenEin)
		)
		(
		:from                   (menu1 "Input...")
		:when                   (Notify )
		:to                     (menu1 "Input...")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_EingabeEin)
		)
		(
		:from                   (menu1 "Load Game...")
		:when                   (Notify )
		:to                     (menu1 "Load Game...")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_LADEN)
		)
		(
		:from                   (menu1 "Save Game...")
		:when                   (Notify )
		:to                     (menu1 "Save Game...")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_SPEICHERN)
		)
		(
		:from                   (menu1 "Show Moves")
		:when                   (Notify )
		:to                     (menu1 "Show Moves")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_Tabelle)
		)
		(
		:from                   (menu1 "Startposition")
		:when                   (Notify )
		:to                     (menu1 "Startposition")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_ZURUECK_ZUM)
		)
		(
		:from                   (menu1 "Endposition")
		:when                   (Notify )
		:to                     (menu1 "Endposition")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_VOR_ZUM)
		)
		(
		:from                   (menu1 "Quit")
		:when                   (Notify )
		:to                     (menu1 "Quit")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_ENDE)
		)
	)
)
(
	:type                   :base-window
	:name                   window
	:owner                  nil
	:width                  407
	:height                 624
	:background-color       ""
	:foreground-color       ""
	:label                  "OTool 0.93.2  (c) Michael Buro     "
	:label-type             :string
	:initial-state          :open
	:show-footer            nil
	:resizable              nil
	:icon-file              ""
	:icon-label             ""
	:icon-mask-file         ""
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   conButtons
	:owner                  window
	:help                   ""
	:x                      0
	:y                      0
	:width                  407
	:height                 92
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            nil
	:menu                   nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   butZurueck
	:owner                  conButtons
	:help                   ""
	:x                      211
	:y                      16
	:width                  45
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Back"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         XCB_Zurueck
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window butZurueck)
		:when                   (Notify )
		:to                     (window butZurueck)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_Zurueck)
		)
	)
)
(
	:type                   :button
	:name                   butVor
	:owner                  conButtons
	:help                   ""
	:x                      260
	:y                      16
	:width                  46
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Next"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         XCB_Vor
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window butVor)
		:when                   (Notify )
		:to                     (window butVor)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_Vor)
		)
	)
)
(
	:type                   :button
	:name                   butOptionen
	:owner                  conButtons
	:help                   ""
	:x                      76
	:y                      28
	:width                  80
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Options"
	:label-type             :string
	:initial-state          :active
	:menu                   menu1
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   butNeu
	:owner                  conButtons
	:help                   ""
	:x                      186
	:y                      40
	:width                  45
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "New"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         XCB_Neu
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window butNeu)
		:when                   (Notify )
		:to                     (window butNeu)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_Neu)
		)
	)
)
(
	:type                   :button
	:name                   butWeiter
	:owner                  conButtons
	:help                   ""
	:x                      235
	:y                      40
	:width                  47
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Cont"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         XCB_Weiter
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window butWeiter)
		:when                   (Notify )
		:to                     (window butWeiter)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_Weiter)
		)
	)
)
(
	:type                   :message
	:name                   message6
	:owner                  conButtons
	:help                   ""
	:x                      268
	:y                      40
	:width                  7
	:height                 13
	:foreground-color       ""
	:label                  "F"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   butStop
	:owner                  conButtons
	:help                   ""
	:x                      286
	:y                      40
	:width                  45
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Stop"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         XCB_Stop
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (window butStop)
		:when                   (Notify )
		:to                     (window butStop)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_Stop)
		)
	)
)
(
	:type                   :message
	:name                   message1
	:owner                  conButtons
	:help                   ""
	:x                      43
	:y                      76
	:width                  8
	:height                 13
	:foreground-color       ""
	:label                  "A"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message2
	:owner                  conButtons
	:help                   ""
	:x                      88
	:y                      76
	:width                  7
	:height                 13
	:foreground-color       ""
	:label                  "B"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message3
	:owner                  conButtons
	:help                   ""
	:x                      132
	:y                      76
	:width                  9
	:height                 13
	:foreground-color       ""
	:label                  "C"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message4
	:owner                  conButtons
	:help                   ""
	:x                      178
	:y                      76
	:width                  9
	:height                 13
	:foreground-color       ""
	:label                  "D"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message5
	:owner                  conButtons
	:help                   ""
	:x                      224
	:y                      76
	:width                  7
	:height                 13
	:foreground-color       ""
	:label                  "E"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message35
	:owner                  conButtons
	:help                   ""
	:x                      268
	:y                      76
	:width                  7
	:height                 13
	:foreground-color       ""
	:label                  "F"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message7
	:owner                  conButtons
	:help                   ""
	:x                      312
	:y                      76
	:width                  9
	:height                 13
	:foreground-color       ""
	:label                  "G"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message32
	:owner                  conButtons
	:help                   ""
	:x                      358
	:y                      76
	:width                  9
	:height                 13
	:foreground-color       ""
	:label                  "H"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   controls3
	:owner                  window
	:help                   ""
	:x                      0
	:y                      92
	:width                  26
	:height                 360
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            nil
	:menu                   nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message9
	:owner                  controls3
	:help                   ""
	:x                      9
	:y                      16
	:width                  8
	:height                 13
	:foreground-color       ""
	:label                  "1"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message10
	:owner                  controls3
	:help                   ""
	:x                      9
	:y                      61
	:width                  8
	:height                 13
	:foreground-color       ""
	:label                  "2"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message11
	:owner                  controls3
	:help                   ""
	:x                      9
	:y                      106
	:width                  8
	:height                 13
	:foreground-color       ""
	:label                  "3"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message12
	:owner                  controls3
	:help                   ""
	:x                      9
	:y                      151
	:width                  8
	:height                 13
	:foreground-color       ""
	:label                  "4"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message13
	:owner                  controls3
	:help                   ""
	:x                      9
	:y                      196
	:width                  8
	:height                 13
	:foreground-color       ""
	:label                  "5"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message14
	:owner                  controls3
	:help                   ""
	:x                      9
	:y                      241
	:width                  8
	:height                 13
	:foreground-color       ""
	:label                  "6"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message15
	:owner                  controls3
	:help                   ""
	:x                      9
	:y                      286
	:width                  8
	:height                 13
	:foreground-color       ""
	:label                  "7"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message16
	:owner                  controls3
	:help                   ""
	:x                      9
	:y                      331
	:width                  8
	:height                 13
	:foreground-color       ""
	:label                  "8"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :canvas-pane
	:name                   canBrett
	:owner                  window
	:help                   ""
	:x                      24
	:y                      92
	:width                  360
	:height                 360
	:background-color       "White"
	:foreground-color       "Black"
	:initial-state          :visible
	:draggable              nil
	:droppable              nil
	:default-drop-site      nil
	:menu                   nil
	:horizontal-scrollbar   nil
	:scrollable-width       360
	:vertical-scrollbar     nil
	:scrollable-height      360
	:repaint-proc           XCB_RepaintBrett
	:event-handler          nil
	:drawing-model          :xview
	:user-data              ()
	:actions                (
		(
		:from                   (window canBrett)
		:when                   (Repaint )
		:to                     (window canBrett)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_RepaintBrett)
		)
		(
		:from                   (window canBrett)
		:when                   (Select )
		:to                     (window canBrett)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_SelectBrett)
		)
	)
)
(
	:type                   :control-area
	:name                   controls4
	:owner                  window
	:help                   ""
	:x                      384
	:y                      92
	:width                  23
	:height                 360
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            nil
	:menu                   nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message17
	:owner                  controls4
	:help                   ""
	:x                      8
	:y                      16
	:width                  8
	:height                 13
	:foreground-color       ""
	:label                  "1"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message18
	:owner                  controls4
	:help                   ""
	:x                      8
	:y                      61
	:width                  8
	:height                 13
	:foreground-color       ""
	:label                  "2"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message19
	:owner                  controls4
	:help                   ""
	:x                      8
	:y                      106
	:width                  8
	:height                 13
	:foreground-color       ""
	:label                  "3"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message20
	:owner                  controls4
	:help                   ""
	:x                      8
	:y                      151
	:width                  8
	:height                 13
	:foreground-color       ""
	:label                  "4"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message21
	:owner                  controls4
	:help                   ""
	:x                      8
	:y                      196
	:width                  8
	:height                 13
	:foreground-color       ""
	:label                  "5"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message22
	:owner                  controls4
	:help                   ""
	:x                      8
	:y                      241
	:width                  8
	:height                 13
	:foreground-color       ""
	:label                  "6"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message23
	:owner                  controls4
	:help                   ""
	:x                      8
	:y                      286
	:width                  8
	:height                 13
	:foreground-color       ""
	:label                  "7"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message24
	:owner                  controls4
	:help                   ""
	:x                      8
	:y                      331
	:width                  8
	:height                 13
	:foreground-color       ""
	:label                  "8"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :control-area
	:name                   conMsg
	:owner                  window
	:help                   ""
	:x                      0
	:y                      452
	:width                  407
	:height                 172
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            nil
	:menu                   nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message25
	:owner                  conMsg
	:help                   ""
	:x                      41
	:y                      8
	:width                  8
	:height                 13
	:foreground-color       ""
	:label                  "A"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message26
	:owner                  conMsg
	:help                   ""
	:x                      86
	:y                      8
	:width                  7
	:height                 13
	:foreground-color       ""
	:label                  "B"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message27
	:owner                  conMsg
	:help                   ""
	:x                      130
	:y                      8
	:width                  9
	:height                 13
	:foreground-color       ""
	:label                  "C"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message28
	:owner                  conMsg
	:help                   ""
	:x                      176
	:y                      8
	:width                  9
	:height                 13
	:foreground-color       ""
	:label                  "D"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message29
	:owner                  conMsg
	:help                   ""
	:x                      222
	:y                      8
	:width                  7
	:height                 13
	:foreground-color       ""
	:label                  "E"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message30
	:owner                  conMsg
	:help                   ""
	:x                      266
	:y                      8
	:width                  7
	:height                 13
	:foreground-color       ""
	:label                  "F"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message31
	:owner                  conMsg
	:help                   ""
	:x                      310
	:y                      8
	:width                  9
	:height                 13
	:foreground-color       ""
	:label                  "G"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message33
	:owner                  conMsg
	:help                   ""
	:x                      356
	:y                      8
	:width                  9
	:height                 13
	:foreground-color       ""
	:label                  "H"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   group1
	:x                      41
	:y                      8
	:group-type             nil
	:members                (message25 message26 message27 message28 message29 message30 message31 message33 )
	:rows                   0
	:row-alignment          :top-edges
	:columns                0
	:column-alignment       :left-edges
	:horizontal-spacing     10
	:vertical-spacing       10
	:anchor-object          nil
	:anchor-point           :north-west
	:reference-point        :north-west
	:horizontal-offset      0
	:vertical-offset        0
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message8
	:owner                  conMsg
	:help                   ""
	:x                      55
	:y                      56
	:width                  37
	:height                 13
	:foreground-color       ""
	:label                  "Black"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   msgPlayers
	:owner                  conMsg
	:help                   ""
	:x                      146
	:y                      56
	:width                  115
	:height                 13
	:foreground-color       ""
	:label                  "BLACK vs. WHITE"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   message34
	:owner                  conMsg
	:help                   ""
	:x                      316
	:y                      56
	:width                  39
	:height                 13
	:foreground-color       ""
	:label                  "White"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   msgZeitB
	:owner                  conMsg
	:help                   ""
	:x                      25
	:y                      88
	:width                  59
	:height                 13
	:foreground-color       ""
	:label                  "-9999:00"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   msgSteineB
	:owner                  conMsg
	:help                   ""
	:x                      105
	:y                      88
	:width                  16
	:height                 13
	:foreground-color       ""
	:label                  "64"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   msgSteine
	:owner                  conMsg
	:help                   ""
	:x                      178
	:y                      88
	:width                  51
	:height                 13
	:foreground-color       ""
	:label                  "64 Discs"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   msgSteineW
	:owner                  conMsg
	:help                   ""
	:x                      290
	:y                      88
	:width                  16
	:height                 13
	:foreground-color       ""
	:label                  "64"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   msgZeitW
	:owner                  conMsg
	:help                   ""
	:x                      322
	:y                      88
	:width                  59
	:height                 13
	:foreground-color       ""
	:label                  "-9999:00"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :group
	:name                   group4
	:x                      290
	:y                      88
	:group-type             nil
	:members                (msgSteineW msgZeitW )
	:rows                   0
	:row-alignment          :top-edges
	:columns                0
	:column-alignment       :left-edges
	:horizontal-spacing     10
	:vertical-spacing       10
	:anchor-object          nil
	:anchor-point           :north-west
	:reference-point        :north-west
	:horizontal-offset      0
	:vertical-offset        0
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   msgZugB
	:owner                  conMsg
	:help                   ""
	:x                      65
	:y                      116
	:width                  17
	:height                 13
	:foreground-color       ""
	:label                  "pa"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   msgZugW
	:owner                  conMsg
	:help                   ""
	:x                      327
	:y                      116
	:width                  17
	:height                 13
	:foreground-color       ""
	:label                  "pa"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   msgBlaBla
	:owner                  conMsg
	:help                   ""
	:x                      159
	:y                      152
	:width                  89
	:height                 13
	:foreground-color       ""
	:label                  "Black has won!"
	:label-type             :string
	:label-bold             nil
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :popup-window
	:name                   popEinstellungen
	:owner                  window
	:width                  294
	:height                 265
	:background-color       ""
	:foreground-color       ""
	:label                  "Einstellungen          "
	:label-type             :string
	:initial-state          :invisible
	:show-footer            nil
	:resizable              nil
	:pinned                 t
	:done-handler           XCB_EinstellungenDone
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (popEinstellungen)
		:when                   (Done )
		:to                     (popEinstellungen)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_EinstellungenDone)
		)
	)
)
(
	:type                   :control-area
	:name                   conEigenschaften
	:owner                  popEinstellungen
	:help                   ""
	:x                      0
	:y                      0
	:width                  294
	:height                 265
	:background-color       ""
	:foreground-color       ""
	:initial-state          :visible
	:show-border            nil
	:menu                   nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   setSpielerB
	:owner                  conEigenschaften
	:help                   ""
	:x                      70
	:y                      15
	:width                  210
	:height                 23
	:value-x                70
	:value-y                15
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  ""
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Channel1" "Channel2" "Human" )
	:choice-label-types     (:string :string :string )
	:choice-colors          ("" "" "" )
	:initial-selections     (nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   msgFarbe1
	:owner                  conEigenschaften
	:help                   ""
	:x                      16
	:y                      32
	:width                  37
	:height                 13
	:foreground-color       ""
	:label                  "Black"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   textRestzeitB
	:owner                  conEigenschaften
	:help                   ""
	:x                      79
	:y                      47
	:width                  192
	:height                 15
	:value-x                200
	:value-y                47
	:value-length           3
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :numeric
	:label                  "Remaining Time"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:max-value              9999
	:min-value              1
	:initial-value          0
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :setting
	:name                   setSpielerW
	:owner                  conEigenschaften
	:help                   ""
	:x                      70
	:y                      77
	:width                  210
	:height                 23
	:value-x                70
	:value-y                77
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  ""
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("Channel1" "Channel2" "Human" )
	:choice-label-types     (:string :string :string )
	:choice-colors          ("" "" "" )
	:initial-selections     (nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :message
	:name                   msgFarbe2
	:owner                  conEigenschaften
	:help                   ""
	:x                      15
	:y                      94
	:width                  39
	:height                 13
	:foreground-color       ""
	:label                  "White"
	:label-type             :string
	:label-bold             t
	:initial-state          :active
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :text-field
	:name                   textRestzeitW
	:owner                  conEigenschaften
	:help                   ""
	:x                      79
	:y                      109
	:width                  192
	:height                 15
	:value-x                200
	:value-y                109
	:value-length           3
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :numeric
	:label                  "Remaining Time"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:max-value              9999
	:min-value              2
	:initial-value          0
	:initial-state          :active
	:read-only              nil
	:notify-handler         nil
	:event-handler          nil
	:user-data              ()
	:actions                ()
)
(
	:type                   :button
	:name                   butWechseln
	:owner                  conEigenschaften
	:help                   ""
	:x                      85
	:y                      160
	:width                  58
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Toggle"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         XCB_EWechseln
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (popEinstellungen butWechseln)
		:when                   (Notify )
		:to                     (popEinstellungen butWechseln)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_EWechseln)
		)
	)
)
(
	:type                   :button
	:name                   butBenutzen
	:owner                  conEigenschaften
	:help                   ""
	:x                      173
	:y                      160
	:width                  36
	:height                 19
	:constant-width         nil
