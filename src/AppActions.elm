module AppActions where


-- Imports
import ItemFeed
import ReminderInput


-- Actions
type Action = NoAction
            | UpdateItemFeed ItemFeed.Action
            | UpdateReminderInput ReminderInput.Action
            | UpdateKeyboardInput KeyboardStateAction
            | ToggleReminderInputView


type KeyboardStateAction  = ToggleAlt
                          | ToggleNext
                          | TogglePrev
                          | ToggleSort
                          | ToggleTruncate
                          | TogglePinned
                          | ToggleDone
                          | ToggleDoneSection
                          | ToggleReminderInput
