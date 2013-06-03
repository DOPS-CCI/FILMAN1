function varargout = AdvancedPlot(varargin)
% ADVANCEDPLOT M-file for AdvancedPlot.fig
%      ADVANCEDPLOT, by itself, creates a new ADVANCEDPLOT or raises the existing
%      singleton*.
%
%      H = ADVANCEDPLOT returns the handle to a new ADVANCEDPLOT or the handle to
%      the existing singleton*.
%
%      ADVANCEDPLOT('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in ADVANCEDPLOT.M with the given input arguments.
%
%      ADVANCEDPLOT('Property','Value',...) creates a new ADVANCEDPLOT or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before AdvancedPlot_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to AdvancedPlot_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help AdvancedPlot

% Last Modified by GUIDE v2.5 03-Dec-2008 15:53:39

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @AdvancedPlot_OpeningFcn, ...
                   'gui_OutputFcn',  @AdvancedPlot_OutputFcn, ...
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


% --- Executes just before AdvancedPlot is made visible.
function AdvancedPlot_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to AdvancedPlot (see VARARGIN)

% Choose default command line output for AdvancedPlot
handles.output = hObject;

n=nargin;
values=varargin;
handles.gvnames=varargin{4};
handles.gvvalues=varargin{6};
set(handles.text1,'String',varargin{2});
set(handles.listbox1,'String',handles.gvnames);
%set(handles.listbox2,'String',handles.gvvalues(1));
set(handles.listbox2,'String','');
set(handles.listbox3,'String','');
s=size(handles.gvnames,2);
handles.cdv(1:s)={{}};
% Update handles structure
handles.header2=varargin{8};
handles.groupvars=varargin{10};
handles.data=varargin{12};
handles.fileinfo=varargin{14};
handles.EdChanFind=varargin{16};
guidata(hObject, handles);

% UIWAIT makes AdvancedPlot wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = AdvancedPlot_OutputFcn(hObject, eventdata, handles) 
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;


% --- Executes on selection change in listbox1.
function listbox1_Callback(hObject, eventdata, handles)
% hObject    handle to listbox1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns listbox1 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from listbox1
c=get(hObject,'Value');
set(handles.listbox2,'String',handles.gvvalues(c));
if isempty(handles.cdv{c})
    set(handles.listbox2,'Value',1);
else
    set(handles.listbox2,'Value',handles.cdv{c});
end
guidata(hObject, handles);


% --- Executes during object creation, after setting all properties.
function listbox1_CreateFcn(hObject, eventdata, handles)
% hObject    handle to listbox1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: listbox controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end

% --- Executes on button press in pushbutton1.
function pushbutton1_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
gvn=get(handles.listbox1,'Value');
gvv=get(handles.listbox2,'Value');
handles.cdv(gvn)={gvv};
S3='';
for i=1:size(handles.gvnames,2)
    if ~isempty(handles.cdv{i})
        c=handles.gvvalues{i};
        c2=c(handles.cdv{i});
        ic2=min(length(c2),70);
        S3=[S3
            {[handles.gvnames{i} ' : ' num2str(c2(1:ic2))]}];
    end
end
set(handles.listbox3,'String',S3);
guidata(hObject, handles);


% --- Executes on button press in pushbutton2.
function pushbutton2_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
s=size(handles.gvnames,2);
handles.cdv(1:s)={{}};
set(handles.listbox3,'String','');
guidata(hObject, handles);


% --- Executes on button press in pushbutton3.
function pushbutton3_Callback(hObject, eventdata, handles)
% hObject    handle to pushbutton3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
rs3(handles.gvnames,handles.gvvalues,handles.cdv,...
    handles.data,handles.fileinfo,...
    handles.header2,handles.groupvars,...
    handles.EdChanFind);


% --- Executes on selection change in listbox2.
function listbox2_Callback(hObject, eventdata, handles)
% hObject    handle to listbox2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns listbox2 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from listbox2


% --- Executes during object creation, after setting all properties.
function listbox2_CreateFcn(hObject, eventdata, handles)
% hObject    handle to listbox2 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: listbox controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


%function AdvancedPlot_CallBack(hObject, eventdata, handles)


% --- Executes on selection change in listbox3.
function listbox3_Callback(hObject, eventdata, handles)
% hObject    handle to listbox3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: contents = get(hObject,'String') returns listbox3 contents as cell array
%        contents{get(hObject,'Value')} returns selected item from listbox3


% --- Executes during object creation, after setting all properties.
function listbox3_CreateFcn(hObject, eventdata, handles)
% hObject    handle to listbox3 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: listbox controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


