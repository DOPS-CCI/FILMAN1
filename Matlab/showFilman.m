function varargout = showFilman(varargin)
% FILMAN M-file for Filman.fig
%      FILMAN, by itself, creates a new FILMAN or raises the existing
%      singleton*.
%
%      H = FILMAN returns the handle to a new FILMAN or the handle to
%      the existing singleton*.
%
%      FILMAN('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in FILMAN.M with the given input arguments.
%
%      FILMAN('Property','Value',...) creates a new FILMAN or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before Filman_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to Filman_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help Filman

% Last Modified by GUIDE v2.5 01-Dec-2008 10:03:09

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @Filman_OpeningFcn, ...
                   'gui_OutputFcn',  @Filman_OutputFcn, ...
                   'gui_LayoutFcn',  [] , ...
                   'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before Filman is made visible.
function Filman_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to Filman (see VARARGIN)

% Choose default command line output for Filman
handles.output = hObject;
set(handles.figure1,'Name','Filman file display');
set(handles.text2,'String','');
set(handles.text3,'String','');
set(handles.text4,'String','');
set(handles.text6,'String','');

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes Filman wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = Filman_OutputFcn(hObject, eventdata, handles) 
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;



% --- Executes on button press in pushbutton1.
function pushbutton1_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
[filename, pathname] = uigetfile({'*.dat','Data files (*.dat)';'*.*','All files (*.*)'}, 'Select input file','C:\EEGDATA\*.dat');
if isequal(filename,0)
   %disp('User selected Cancel')
   set(handles.uipanel1,'Title','File info');
else
   %disp(['User selected ', fullfile(pathname, filename)])
   visualizeFilman(hObject,eventdata,handles,pathname,filename);
end

