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
	:menu-item-labels       ("Einstellungen..." "Eingabe..." "Spiel laden..." "Spiel speichern..." "Tabelle anzeigen" "Zurück bis Anfang" "Vor bis Ende" "Programmende" )
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
		:from                   (menu1 "Einstellungen...")
		:when                   (Notify )
		:to                     (menu1 "Einstellungen...")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_EinstellungenEin)
		)
		(
		:from                   (menu1 "Eingabe...")
		:when                   (Notify )
		:to                     (menu1 "Eingabe...")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_EingabeEin)
		)
		(
		:from                   (menu1 "Spiel laden...")
		:when                   (Notify )
		:to                     (menu1 "Spiel laden...")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_LADEN)
		)
		(
		:from                   (menu1 "Spiel speichern...")
		:when                   (Notify )
		:to                     (menu1 "Spiel speichern...")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_SPEICHERN)
		)
		(
		:from                   (menu1 "Tabelle anzeigen")
		:when                   (Notify )
		:to                     (menu1 "Tabelle anzeigen")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_Tabelle)
		)
		(
		:from                   (menu1 "Zurück bis Anfang")
		:when                   (Notify )
		:to                     (menu1 "Zurück bis Anfang")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_ZURUECK_ZUM)
		)
		(
		:from                   (menu1 "Vor bis Ende")
		:when                   (Notify )
		:to                     (menu1 "Vor bis Ende")
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_VOR_ZUM)
		)
		(
		:from                   (menu1 "Programmende")
		:when                   (Notify )
		:to                     (menu1 "Programmende")
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
	:x                      207
	:y                      16
	:width                  60
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Zurück"
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
	:x                      271
	:y                      16
	:width                  38
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Vor"
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
	:x                      74
	:y                      28
	:width                  89
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Optionen"
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
	:x                      182
	:y                      41
	:width                  42
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Neu"
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
	:x                      227
	:y                      41
	:width                  57
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Weiter"
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
	:type                   :button
	:name                   butStop
	:owner                  conButtons
	:help                   ""
	:x                      287
	:y                      41
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
	:name                   message6
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
	:x                      43
	:y                      16
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
	:x                      88
	:y                      16
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
	:x                      132
	:y                      16
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
	:x                      178
	:y                      16
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
	:x                      224
	:y                      16
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
	:x                      268
	:y                      16
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
	:x                      312
	:y                      16
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
	:x                      358
	:y                      16
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
	:type                   :message
	:name                   message8
	:owner                  conMsg
	:help                   ""
	:x                      42
	:y                      56
	:width                  56
	:height                 13
	:foreground-color       ""
	:label                  "Schwarz"
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
	:width                  34
	:height                 13
	:foreground-color       ""
	:label                  "Weiß"
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
	:x                      22
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
	:x                      102
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
	:x                      176
	:y                      88
	:width                  58
	:height                 13
	:foreground-color       ""
	:label                  "64 Steine"
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
	:x                      288
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
	:x                      320
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
	:name                   msgZugB
	:owner                  conMsg
	:help                   ""
	:x                      62
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
	:x                      325
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
	:x                      131
	:y                      152
	:width                  145
	:height                 13
	:foreground-color       ""
	:label                  "Schwarz hat gewonnen!"
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
	:x                      93
	:y                      16
	:width                  182
	:height                 23
	:value-x                93
	:value-y                16
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
	:choices                ("Comp1" "Comp2" "Mensch" )
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
	:x                      21
	:y                      30
	:width                  56
	:height                 13
	:foreground-color       ""
	:label                  "Schwarz"
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
	:x                      117
	:y                      46
	:width                  134
	:height                 15
	:value-x                180
	:value-y                46
	:value-length           3
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :numeric
	:label                  "Restzeit"
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
	:type                   :setting
	:name                   setSpielerW
	:owner                  conEigenschaften
	:help                   ""
	:x                      93
	:y                      86
	:width                  182
	:height                 23
	:value-x                93
	:value-y                86
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
	:choices                ("Comp1" "Comp2" "Mensch" )
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
	:x                      32
	:y                      102
	:width                  34
	:height                 13
	:foreground-color       ""
	:label                  "Weiß"
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
	:x                      117
	:y                      116
	:width                  134
	:height                 15
	:value-x                180
	:value-y                116
	:value-length           3
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :numeric
	:label                  "Restzeit"
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
	:x                      55
	:y                      160
	:width                  75
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Wechseln"
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
	:x                      143
	:y                      160
	:width                  97
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Übernehmen"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         XCB_EINST_BEN
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (popEinstellungen butBenutzen)
		:when                   (Notify )
		:to                     (popEinstellungen butBenutzen)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_EINST_BEN)
		)
	)
)
(
	:type                   :setting
	:name                   setMoegl
	:owner                  conEigenschaften
	:help                   ""
	:x                      38
	:y                      204
	:width                  223
	:height                 23
	:value-x                243
	:value-y                204
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :check
	:selection-required     t
	:label                  "  Zugmöglichkeiten anzeigen"
	:label-type             :string
	:notify-handler         XCB_Moegl
	:event-handler          nil
	:choices                ("" )
	:choice-label-types     (:string )
	:choice-colors          ("" )
	:initial-selections     (nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (popEinstellungen setMoegl)
		:when                   (Notify )
		:to                     (popEinstellungen setMoegl)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_Moegl)
		)
	)
)
(
	:type                   :setting
	:name                   setBeep
	:owner                  conEigenschaften
	:help                   ""
	:x                      18
	:y                      232
	:width                  243
	:height                 23
	:value-x                243
	:value-y                232
	:rows                   0
	:columns                1
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :check
	:selection-required     t
	:label                  "Computerzug akustisch melden"
	:label-type             :string
	:notify-handler         nil
	:event-handler          nil
	:choices                ("" )
	:choice-label-types     (:string )
	:choice-colors          ("" )
	:initial-selections     (nil )
	:initial-state          :active
	:user-data              ()
	:actions                ()
)
(
	:type                   :popup-window
	:name                   popEingabe
	:owner                  window
	:width                  251
	:height                 239
	:background-color       ""
	:foreground-color       ""
	:label                  "Eingabe        "
	:label-type             :string
	:initial-state          :invisible
	:show-footer            nil
	:resizable              nil
	:pinned                 t
	:done-handler           XCB_EingabeDone
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (popEingabe)
		:when                   (Done )
		:to                     (popEingabe)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_EingabeDone)
		)
	)
)
(
	:type                   :control-area
	:name                   controls2
	:owner                  popEingabe
	:help                   ""
	:x                      0
	:y                      0
	:width                  251
	:height                 239
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
	:name                   setAmZug
	:owner                  controls2
	:help                   ""
	:x                      33
	:y                      16
	:width                  175
	:height                 23
	:value-x                95
	:value-y                16
	:rows                   1
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Am Zug:"
	:label-type             :string
	:notify-handler         XCB_AmZug
	:event-handler          nil
	:choices                ("Schwarz" "Weiß" )
	:choice-label-types     (:string :string )
	:choice-colors          ("" "" )
	:initial-selections     (nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (popEingabe setAmZug)
		:when                   (Notify )
		:to                     (popEingabe setAmZug)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_AmZug)
		)
	)
)
(
	:type                   :setting
	:name                   setSetzModus
	:owner                  controls2
	:help                   ""
	:x                      10
	:y                      48
	:width                  231
	:height                 46
	:value-x                95
	:value-y                48
	:rows                   2
	:columns                0
	:layout-type            :horizontal
	:foreground-color       ""
	:setting-type           :exclusive
	:selection-required     t
	:label                  "Setzmodus:"
	:label-type             :string
	:notify-handler         XCB_Setzmodus
	:event-handler          nil
	:choices                ("Schwarz" "Weiß" "Wechseln" "Leer" )
	:choice-label-types     (:string :string :string :string )
	:choice-colors          ("" "" "" "" )
	:initial-selections     (nil nil nil nil )
	:initial-state          :active
	:user-data              ()
	:actions                (
		(
		:from                   (popEingabe setSetzModus)
		:when                   (Notify )
		:to                     (popEingabe setSetzModus)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_Setzmodus)
		)
	)
)
(
	:type                   :button
	:name                   butDrehen
	:owner                  controls2
	:help                   ""
	:x                      21
	:y                      112
	:width                  62
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Drehen"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         XCB_Drehen
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (popEingabe butDrehen)
		:when                   (Notify )
		:to                     (popEingabe butDrehen)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_Drehen)
		)
	)
)
(
	:type                   :button
	:name                   butSpiegeln
	:owner                  controls2
	:help                   ""
	:x                      93
	:y                      112
	:width                  70
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Spiegeln"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         XCB_Spiegeln
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (popEingabe butSpiegeln)
		:when                   (Notify )
		:to                     (popEingabe butSpiegeln)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_Spiegeln)
		)
	)
)
(
	:type                   :button
	:name                   butFuellen
	:owner                  controls2
	:help                   ""
	:x                      173
	:y                      112
	:width                  56
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Füllen"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         XCB_Fuellen
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (popEingabe butFuellen)
		:when                   (Notify )
		:to                     (popEingabe butFuellen)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_Fuellen)
		)
	)
)
(
	:type                   :button
	:name                   butGrund
	:owner                  controls2
	:help                   ""
	:x                      73
	:y                      144
	:width                  105
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Grundstellung"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         XCB_Grund
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (popEingabe butGrund)
		:when                   (Notify )
		:to                     (popEingabe butGrund)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_Grund)
		)
	)
)
(
	:type                   :button
	:name                   butEingabeOk
	:owner                  controls2
	:help                   ""
	:x                      77
	:y                      204
	:width                  97
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Übernehmen"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         XCB_EingabeOk
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (popEingabe butEingabeOk)
		:when                   (Notify )
		:to                     (popEingabe butEingabeOk)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_EingabeOk)
		)
	)
)
(
	:type                   :popup-window
	:name                   popLaden
	:owner                  nil
	:width                  242
	:height                 74
	:background-color       ""
	:foreground-color       ""
	:label                  "Spiel laden         "
	:label-type             :string
	:initial-state          :invisible
	:show-footer            nil
	:resizable              nil
	:pinned                 nil
	:done-handler           XCB_LADEN_DONE
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (popLaden)
		:when                   (Done )
		:to                     (popLaden)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_LADEN_DONE)
		)
	)
)
(
	:type                   :control-area
	:name                   controls1
	:owner                  popLaden
	:help                   ""
	:x                      0
	:y                      0
	:width                  242
	:height                 74
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
	:type                   :text-field
	:name                   textLadenName
	:owner                  controls1
	:help                   ""
	:x                      24
	:y                      12
	:width                  194
	:height                 15
	:value-x                74
	:value-y                12
	:value-length           18
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Name:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              nil
	:notify-handler         XCB_TEXT_LADEN
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (popLaden textLadenName)
		:when                   (Notify )
		:to                     (popLaden textLadenName)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_TEXT_LADEN)
		)
	)
)
(
	:type                   :button
	:name                   butSpielLaden
	:owner                  controls1
	:help                   ""
	:x                      94
	:y                      48
	:width                  54
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Laden"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         XCB_SPIEL_LADEN
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (popLaden butSpielLaden)
		:when                   (Notify )
		:to                     (popLaden butSpielLaden)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_SPIEL_LADEN)
		)
	)
)
(
	:type                   :popup-window
	:name                   popSpeichern
	:owner                  nil
	:width                  242
	:height                 74
	:background-color       ""
	:foreground-color       ""
	:label                  "Spiel speichern         "
	:label-type             :string
	:initial-state          :invisible
	:show-footer            nil
	:resizable              nil
	:pinned                 nil
	:done-handler           XCB_SPEICHERN_DONE
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (popSpeichern)
		:when                   (Done )
		:to                     (popSpeichern)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_SPEICHERN_DONE)
		)
	)
)
(
	:type                   :control-area
	:name                   controls5
	:owner                  popSpeichern
	:help                   ""
	:x                      0
	:y                      0
	:width                  242
	:height                 74
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
	:type                   :text-field
	:name                   textSpeichernName
	:owner                  controls5
	:help                   ""
	:x                      24
	:y                      12
	:width                  194
	:height                 15
	:value-x                74
	:value-y                12
	:value-length           18
	:stored-length          80
	:rows                   3
	:foreground-color       ""
	:text-type              :alphanumeric
	:label                  "Name:"
	:label-type             :string
	:layout-type            :horizontal
	:value-underlined       t
	:initial-value          ""
	:initial-state          :active
	:read-only              nil
	:notify-handler         XCB_TEXT_SPEICHERN
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (popSpeichern textSpeichernName)
		:when                   (Notify )
		:to                     (popSpeichern textSpeichernName)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_TEXT_SPEICHERN)
		)
	)
)
(
	:type                   :button
	:name                   butSpielSpeichern
	:owner                  controls5
	:help                   ""
	:x                      82
	:y                      48
	:width                  78
	:height                 19
	:constant-width         nil
	:button-type            :normal
	:foreground-color       ""
	:label                  "Speichern"
	:label-type             :string
	:initial-state          :active
	:menu                   nil
	:notify-handler         XCB_SPIEL_SPEICHERN
	:event-handler          nil
	:user-data              ()
	:actions                (
		(
		:from                   (popSpeichern butSpielSpeichern)
		:when                   (Notify )
		:to                     (popSpeichern butSpielSpeichern)
		:function_type          CallFunction
		:arg_type               ()
		:action                 (XCB_SPIEL_SPEICHERN)
		)
	)
)
)
