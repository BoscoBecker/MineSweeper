unit View.Share;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia, Vcl.Skia,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TFormShare = class(TForm)
    ImageFacebook: TImage;
    ImageLinkedin: TImage;
    ImageEmail: TImage;
    ImageWhatsapp: TImage;
    LabelFacebook: TSkLabel;
    LabelLinkedin: TSkLabel;
    LabelMail: TSkLabel;
    EditURL: TEdit;
    PanelCloud: TPanel;
    ImageCloud: TSkAnimatedImage;
    LabelShare: TSkLabel;
    ImageClose: TSkAnimatedImage;
    LabelCopyLink: TSkLabel;
    LabelCopied: TSkLabel;
    procedure ImageFacebookClick(Sender: TObject);
    procedure ImageWhatsappClick(Sender: TObject);
    procedure ImageLinkedinClick(Sender: TObject);
    procedure ImageEmailClick(Sender: TObject);
    procedure EditURLClick(Sender: TObject);
    procedure ImageCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ImageFacebookMouseEnter(Sender: TObject);
    procedure ImageWhatsappMouseEnter(Sender: TObject);
    procedure ImageLinkedinMouseEnter(Sender: TObject);
    procedure ImageEmailMouseEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Rgn: HRGN;
    procedure BorderRadiusEffect;
    procedure CopyToClipborad;
    procedure ShakeImage(const Image : Timage);
    procedure ShareOnFacebook;
    procedure ShareOnLinkedIn;
    procedure ShareOnWhatsApp;
    procedure ShareByEmail;
  end;

var
  FormShare: TFormShare;

implementation

{$R *.dfm}

uses ShellAPI, System.Threading, Clipbrd, Math;

procedure TFormShare.BorderRadiusEffect;
begin
  Rgn:= CreateRoundRectRgn(0, 0, Width, Height, 30, 30);
  SetWindowRgn(Handle, Rgn, True);
end;

procedure TFormShare.CopyToClipborad;
begin
  Clipboard.AsText := EditURL.Text;
  LabelCopied.Visible:= True;
end;

procedure TFormShare.EditURLClick(Sender: TObject);
begin
  CopyToClipborad;
end;

procedure TFormShare.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DeleteObject(Rgn);
  Action:= TCloseAction.caFree;
end;

procedure TFormShare.FormCreate(Sender: TObject);
begin
  BorderRadiusEffect;
end;

procedure TFormShare.ImageCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormShare.ImageEmailClick(Sender: TObject);
begin
  ShareByEmail;
end;

procedure TFormShare.ImageEmailMouseEnter(Sender: TObject);
begin
  ShakeImage(ImageEmail);
end;

procedure TFormShare.ImageFacebookClick(Sender: TObject);
begin
  ShareOnFacebook;
end;

procedure TFormShare.ImageFacebookMouseEnter(Sender: TObject);
begin
  ShakeImage(ImageFacebook);
end;

procedure TFormShare.ImageLinkedinClick(Sender: TObject);
begin
  ShareOnLinkedIn;
end;

procedure TFormShare.ImageLinkedinMouseEnter(Sender: TObject);
begin
  ShakeImage(ImageLinkedin);
end;

procedure TFormShare.ImageWhatsappClick(Sender: TObject);
begin
  ShareOnWhatsApp;
end;

procedure TFormShare.ImageWhatsappMouseEnter(Sender: TObject);
begin
  ShakeImage(ImageWhatsapp);
end;

procedure TFormShare.ShakeImage(const Image: Timage);
begin
  TTask.Run(procedure
  begin
    var OrigLeft := Image.Left;
    var OrigTop := Image.Top;
    for var i := 1 to 10 do
    begin
      Image.Left := OrigLeft + RandomRange(-3, 3);
      Image.Top := OrigTop + RandomRange(-3, 3);
      Sleep(50);
    end;
    Image.Left := OrigLeft;
    Image.Top := OrigTop;
  end);
end;

procedure TFormShare.ShareByEmail;
begin
  TTask.Run(procedure begin ShellExecute(0, 'open', 'mailto:?subject=Jogue%20o%20meu%20MineSweeper!&body=Confira%20este%20jogo:%20https://minesweeper.boscobecker.fun', nil, nil, SW_SHOWNORMAL); end);
end;

procedure TFormShare.ShareOnFacebook;
begin
  TTask.Run(procedure begin  ShellExecute(0, 'open', 'https://www.facebook.com/sharer/sharer.php?u=https://minesweeper.boscobecker.fun', nil, nil, SW_SHOWNORMAL); end);
end;

procedure TFormShare.ShareOnLinkedIn;
begin
  TTask.Run(procedure begin ShellExecute(0, 'open', 'https://www.linkedin.com/sharing/share-offsite/?url=https://minesweeper.boscobecker.fun', nil, nil, SW_SHOWNORMAL); end);
end;

procedure TFormShare.ShareOnWhatsApp;
begin
  TTask.Run(procedure begin  ShellExecute(0, 'open', 'https://www.linkedin.com/sharing/share-offsite/?url=https://minesweeper.boscobecker.fun', nil, nil, SW_SHOWNORMAL); end);
end;


end.
