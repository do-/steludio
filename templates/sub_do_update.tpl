
	$_REQUEST {_label} or croak "#_label#:�� ������ ������ ������������";
	
#	$_REQUEST {_id_user} or croak "#_id_user#:�� ������ ������� ������������";

#	$_REQUEST {_id_org} = sql_select_id (orgs => {fake => 0, label => $_REQUEST {_id_org__label}},
#		['label'],
#		sub {vb_yes ("����������� '$_REQUEST{_id_org__label}' ���� ��� � �����������. �������� � ����� ������?")},
#	) or croak '#_id_org#:�������� ��������';

	do_update_DEFAULT ();