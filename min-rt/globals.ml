open MiniMLRuntime;;

(**************** ��違�㏍�若�����紊���違�勐㐂荐� ****************)

(* ��ŝ����吾�с�壔����勉����若�帥����ャ����������壔����̩�����紊�60���鐚�*)
let objects = 
  let dummy = Array.make 0 0.0 in
  Array.make (60) 
    (0, 0, 0, 0, 
     dummy, dummy,
     false, dummy, dummy,
     dummy)

(* [| x荵吾�勤軌��紫����㋛�� , y荵吾�勤軌��紫����㋛�� |] *)
let size = Array.make 2 128

(* 絎�茵������ŝ����激�с��: �����������医�阪����勖����� *)
let dbg = Array.make 1 true
(* Screen ��勐婚罔� *)
let screen = Array.make 3 0.0
(* 荀���鴻�勐婚罔� (offset ��ŝ��) *)
let vp = Array.make 3 0.0
(* 荀���鴻�勐婚罔� (screen 篏�臀勐����� offset ������) *)
let view = Array.make 3 0.0
(* ���羣���劫�������壔����� (���篏������壔�����) *)
let light = Array.make 3 0.0
(* ��鴻�壔�ŝ�若�潟�勐��荵∽�劫��: 筝�茹���∽�違�勐�ゃ�т����� *)
let cos_v = Array.make 2 0.0
let sin_v = Array.make 2 0.0
(* �傈�≪����ゃ�í�ゃ��綣桁墾 (罔�羣�=255) *)
let beam = Array.make 1 255.0
(* AND �����������壔�若�壔��篆���� *)
let and_net = Array.make 50 (Array.make 1 (-1))
(* OR �����������壔�若�壔��篆���� *)
let or_net = Array.make 1 (Array.make 1 (and_net.(0)))

(* reader *)
let temp = Array.make 14 0.0 (* read_nth_object �����勌��罐㊤����� *)
let cs_temp = Array.make 16 0.0

(* solver *)
(**** Callee �����勰��篆∞����違�㏍�若�����紊���� ****)
(* 篋ょ�� ��� t ��勐�� *)
let solver_dist = Array.make 1 0.0

(* ��鴻�㏍�ｃ�潟�勖�劫�� *)
let vscan = Array.make 3 0.0
(* 篋ょ�鴻�勛�贋�剛��茵◒�≪�с�勖�劫�� *)
let intsec_rectside = Array.make 1 0
(* ��肴��������篋ょ�鴻�勖��絨���� t *)
let tmin = Array.make 1 (1000000000.0)
(* 篋ょ�鴻�勐婚罔� *)
let crashed_point = Array.make 3 0.0
(* 茵�腦���������ŝ����吾�с�壔�� *)
let crashed_object = Array.make 1 0
(* 1��ゃ�� AND �����������壔�若�壔�˨�ゃ����⓾�勛��篋������í�� *)
let end_flag = Array.make 1 false
(* �����㋘�若�拷��紮���� *)
let viewpoint = Array.make 3 0.0
(* 羈�膩������壔����� *)
let nvector = Array.make 3 0.0
(* ��鴻�壔�ŝ�若�割����勛�鴻�勖�������� *)
let rgb = Array.make 3 0.0
(* 篋ょ�鴻�勤�� *)
let texture_color = Array.make 3 0.0

(* ��ŝ����吾�с�壔��筝㊤����������鴻�˨�����荀���鴻����壔����� *)
let solver_w_vec = Array.make 3 0.0

(* check_all_inside ���綣���違����壔����� *)
let chkinside_p = Array.make 3 0.0

(* is_outside �����������ñ�� (筝㊤��綏勐��) �����壔����� *)
let isoutside_q = Array.make 3 0.0

(* ��違�㏍�若����˨����������冴�������㏍�若�˨��紊���� *)
(* nvector *)
let nvector_w = Array.make 3 0.0

(* main *)
let scan_d = Array.make 1 0.0
let scan_offset = Array.make 1 0.0
let scan_sscany = Array.make 1 0.0
let scan_met1 = Array.make 1 0.0
let wscan = Array.make 3 0.0
