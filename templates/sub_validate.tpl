
	$_REQUEST {_label} or return "#_label#:�� ������ ������ ������������";
#	$_REQUEST {_id_user} or return "#_id_user#:�� ������ ������� ������������";

#	$_REQUEST {_id_org} = sql_select_id (orgs => {fake => 0, label => $_REQUEST {_id_org__label}},
#		['label'],
#		sub {vb_yes ("����������� '$_REQUEST{_id_org__label}' ���� ��� � �����������. �������� � ����� ������?")},
#	) or return '#_id_org#:�������� ��������';

	undef;
	