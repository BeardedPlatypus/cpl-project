module AppActions where

import ItemFeed
import ReminderInput

import KeyboardActions

-- Actions
type Action = NoAction
            | UpdateItemFeed ItemFeed.Action
            | UpdateReminderInput ReminderInput.Action
            | UpdateKeyboardInput KeyboardActions.Action
