Public Class Form1

   Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
      ' Create two buttons to use as the accept and cancel buttons. 
      Dim button1 As New Button()
      Dim button2 As New Button()
      ' Set the text of button1 to "OK".
      button1.Text = "OK"
      ' Set the position of the button on the form.
      button1.Location = New Point(10, 10)
      ' Set the text of button2 to "Cancel".
      button2.Text = "Cancel"
      ' Set the position of the button based on the location of button1.
      button2.Location = _
         New Point(button1.Left, button1.Height + button1.Top + 10)
      ' Set the caption bar text of the form.   
      Me.Text = "tutorialspoint.com"
      ' Display a help button on the form.
      Me.HelpButton = True
     ' Define the border style of the form to a dialog box.
      Me.FormBorderStyle = FormBorderStyle.FixedDialog
      ' Set the MaximizeBox to false to remove the maximize box.
      Me.MaximizeBox = False
      ' Set the MinimizeBox to false to remove the minimize box.
      Me.MinimizeBox = False
      ' Set the accept button of the form to button1.
      Me.AcceptButton = button1
      ' Set the cancel button of the form to button2.
      Me.CancelButton = button2
      ' Set the start position of the form to the center of the screen.
      Me.StartPosition = FormStartPosition.CenterScreen
      ' Set window width and height
      Me.Height = 300
      Me.Width = 560
      ' Add button1 to the form.
      Me.Controls.Add(button1)
      ' Add button2 to the form.
      Me.Controls.Add(button2)
   End Sub
End Class
