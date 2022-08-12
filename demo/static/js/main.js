const nod_id=['#node_D','#node_E','#node_F','#node_G']
const edg_id=['DE','EF','DF','DG','EG','FG']
const edg_id_r=['ED','FE','FD','GD','GE','GF']
const fad_time=300
const last_time=45*1000
const w_pow=0.9
const actmax=6
const edg_nod=[[0,1],[1,2],[0,2],[0,3],[1,3],[2,3]]
const check_keys=[true,false,true,true,true,false];
const bonus_cent=5
const myOpList={Acylic:{Three:["Acyclic1","Acyclic2","Acyclic3","Acyclic7"],Four:["Acyclic4","Acyclic5","Acyclic6","Acyclic8"]},
				Cyclic:{Three:["Cyclic1","Cyclic2","Cyclic3","Cyclic7"],Four:["Cyclic4","Cyclic5","Cyclic6","Cyclic8"]},
				Unlinked:{Three:["Unlinked1"],Four:["Unlinked2"]}}

const myOpOrder=["Acyclic1","Acyclic2","Acyclic3","Cyclic1","Cyclic2","Cyclic3",
				 "Unlinked1","Acyclic7","Cyclic7",
				 "Acyclic4","Acyclic5","Acyclic6","Cyclic4","Cyclic5","Cyclic6",
				 "Unlinked2","Acyclic8","Cyclic8"
				 ]

var myClick=shuffle([1,2,3])//0=non-causal; 1=AB/BC/AC; 2=BA/CB/CA; 3=loop
myClick.push(0)


var myCon='Reliable'
var timing_num=timing_r.length


//change according to the node numbers
var nod_count=[0,0,0] //to count which progress is going on
var edg_count=[0,0,0]
var blc_mod=[0,0,0]
var edg_nod_pos=[[],[],[]]
var e_confirm=[0,0,0]
var pos=[0,1,2]
var locktime=0


//others
var act_count=0
var ongoing_mod=0
var bonus_time=0
var finish_mod=0
var trial=0 
var struc=[]
var edges=[]//ground strcture in edge (position considered)
var edges_pos=[]//ground structure (position considered)
var t0=0
var myEve=new Array
var myJud=new Array
var myTrialInfo=new Array
var myScore=new Array


function draw_net3(){
	var a=230//distance between nodes
	var d=55//width of nodes (including border)
	$('#pg_game').css({'width':Number(a+d)+'px','height':Number(a*1.732/2+d)+'px',
						'margin-left':'auto','margin-right':'auto','padding':'0px',
						'position':'relative'})

	$('#node_G').css({'display':'none'})
	$('#fr_edge_EG').css({'display':'none'})
	$('#fr_edge_DG').css({'display':'none'})
	$('#fr_edge_FG').css({'display':'none'})

	var m=2 //border of nodes
	var e_p=10 //padding of edge
	var e_h=4 // height of edge
	var a_w_p1=25 // size of arrow para2
	var a_w_p2=35 //size of arrow para1
	$('.node').css({'width':Number(d-2*m)+'px','height':Number(d-2*m)+'px','border-radius':Number(d-2*m)+'px','line-height':Number(d-2*m)+'px'})

	$('#node_D').css({'left':'0px','top':Number(a*1.732/2)+'px'})
	$('#node_E').css({'left':Number(a/2+m)+'px','top':'0px'})
	$('#node_F').css({'left':Number(a+m)+'px','top':Number(a*1.732/2)+'px'})

	for (var i=0;i<edg_id.length;i++){
		$('#fr_edge_'+edg_id[i]).css({'width':Number(a-d)+'px','height':Number(d-2*m)+'px'})
		$('#edge_'+edg_id[i]).css({'width': Number(a-d-2*e_p)+'px','height':e_h+'px','margin-left':e_p+'px','margin-top':Number((d-2*m)/2-e_h/2)+'px'})
		$('#arrow_'+edg_id[i]).css({'margin-top':Number((d-2*m)/2-a_w_p1/2)+'px','border-top': a_w_p1/2+'px solid transparent','border-bottom':a_w_p1/2+'px solid transparent'})
		$('#arrow_'+edg_id_r[i]).css({'margin-top':Number((d-2*m)/2-a_w_p1/2)+'px','border-top': a_w_p1/2+'px solid transparent','border-bottom':a_w_p1/2+'px solid transparent'})

		$('#arrow_'+edg_id[i]).css({'border-left': a_w_p2+'px solid','margin-left':Number(e_p+a-d-2*e_p-a_w_p2+6)+'px'})
		$('#arrow_'+edg_id_r[i]).css({'border-right': a_w_p2+'px solid','margin-left':Number(e_p-6)+'px'})
	}
	
	$('#fr_edge_DF').css({'transform': 'rotate(0deg)','left':d+'px','top':Number(a*1.732/2)+'px'})
	$('#fr_edge_EF').css({'transform': 'rotate(60deg)','left':Number(a*3/4+d*1/2-(a-d)*1/2)+'px','top':Number(a*1.732/4)+'px'})
	$('#fr_edge_DE').css({'transform': 'rotate(-60deg)','left':Number(a*1/4+d*1/2-(a-d)*1/2)+'px','top':Number(a*1.732/4)+'px'})

	var fb_a_w=40
	var fb_e_h=8

	for(var i=0;i<edg_id.length;i++){
		$('#edge_'+edg_id[i]+'_fb').css({'width': Number(a-d-2*e_p)+'px','height':fb_e_h+'px','margin-left':e_p+'px','margin-top':Number((d-2*m)/2-fb_e_h/2)+'px'})
		$('#arrow_'+edg_id[i]+'_fb').css({'margin-top':Number((d-2*m)/2-fb_a_w/2)+'px','border-top': fb_a_w/2+'px solid transparent','border-bottom':fb_a_w/2+'px solid transparent'})
		$('#arrow_'+edg_id_r[i]+'_fb').css({'margin-top':Number((d-2*m)/2-fb_a_w/2)+'px','border-top': fb_a_w/2+'px solid transparent','border-bottom':fb_a_w/2+'px solid transparent'})
	
		$('#arrow_'+edg_id[i]+'_fb').css({'border-left': fb_a_w+'px solid white','margin-left':Number(e_p+a-d-2*e_p-fb_a_w+8)+'px'})
		$('#arrow_'+edg_id_r[i]+'_fb').css({'border-right': fb_a_w+'px solid white','margin-left':Number(e_p-8)+'px'})

	}

	//$('#btn_confirm').css({'top':'55%'})
	//$('.hint').html('?---------?')
}


function draw_net4(){
	var a=260//distance between nodes
	var d=55//width of nodes (including border)
	$('#pg_game').css({'width':Number(a+d)+'px','height':Number(a+d)+'px',
						'margin-left':'auto','margin-right':'auto','padding':'0px',
						'position':'relative'})
	
	$('#node_G').css({'display':'block'})
	$('#fr_edge_EG').css({'display':'block'})
	$('#fr_edge_DG').css({'display':'block'})
	$('#fr_edge_FG').css({'display':'block'})

	var m=2 //border of nodes
	var e_p=10 //padding of edge
	var e_h=4 // height of edge
	var a_w_p1=25 // size of arrow para2
	var a_w_p2=35 //size of arrow para1
	$('.node').css({'width':Number(d-2*m)+'px','height':Number(d-2*m)+'px','border-radius':Number(d-2*m)+'px','line-height':Number(d-2*m)+'px'})

	$('#node_D').css({'left':'0px','top':Number(a/2)+'px'})
	$('#node_E').css({'left':Number(a/2+m)+'px','top':'0px'})
	$('#node_F').css({'left':Number(a+m)+'px','top':Number(a/m)+'px'})
	$('#node_G').css({'left':Number(a/2+m)+'px','top':Number(a+m)+'px'})


	for (var i=0;i<edg_id.length;i++){
		$('#fr_edge_'+edg_id[i]).css({'height':Number(d-2*m)+'px'})

		$('#arrow_'+edg_id[i]).css({'margin-top':Number((d-2*m)/2-a_w_p1/2)+'px','border-top': a_w_p1/2+'px solid transparent','border-bottom':a_w_p1/2+'px solid transparent'})
		$('#arrow_'+edg_id_r[i]).css({'margin-top':Number((d-2*m)/2-a_w_p1/2)+'px','border-top': a_w_p1/2+'px solid transparent','border-bottom':a_w_p1/2+'px solid transparent'})

		$('#arrow_'+edg_id_r[i]).css({'border-right': a_w_p2+'px solid','margin-left':Number(e_p-6)+'px'})
	}

	var e_w1=a-d //edge width -- long one 
	var e_w2=a*1.414/2-d //edge width -- short one

	$('#fr_edge_DF').css({'transform': 'rotate(0deg)','left':d+'px','top':Number(a/2+m)+'px','width':Number(e_w1)+'px'})
	$('#fr_edge_EG').css({'transform': 'rotate(90deg)','left':d+'px','top':Number(a/2+m)+'px','width':Number(e_w1)+'px'})
	$('#fr_edge_DE').css({'transform': 'rotate(-45deg)','left':Number(a*1/4+d*1/2-e_w2/2)+'px','top':Number(a/4)+'px','width':e_w2+'px'})
	$('#fr_edge_EF').css({'transform': 'rotate(45deg)','left':Number(a*3/4+d*1/2-e_w2/2)+'px','top':Number(a/4)+'px','width':e_w2+'px'})
	$('#fr_edge_DG').css({'transform': 'rotate(45deg)','left':Number(a*1/4+d*1/2-e_w2/2)+'px','top':Number(a*3/4)+'px','width':e_w2+'px'})
	$('#fr_edge_FG').css({'transform': 'rotate(135deg)','left':Number(a*3/4+d*1/2-e_w2/2)+'px','top':Number(a*3/4)+'px','width':e_w2+'px'})

	var edg_id1=['DF','EG'];var edg_id2=['DE','EF','DG','FG']

	for (i=0;i<edg_id1.length;i++){
		$('#arrow_'+edg_id1[i]).css({'border-left': a_w_p2+'px solid','margin-left':Number(e_p+e_w1-2*e_p-a_w_p2+6)+'px'})
		$('#edge_'+edg_id1[i]).css({'width': Number(e_w1-2*e_p)+'px','height':e_h+'px','margin-left':e_p+'px','margin-top':Number((d-2*m)/2-e_h/2)+'px'})
	}

	for (i=0;i<edg_id2.length;i++){
		$('#arrow_'+edg_id2[i]).css({'border-left': a_w_p2+'px solid','margin-left':Number(e_p+e_w2-2*e_p-a_w_p2+6)+'px'})
		$('#edge_'+edg_id2[i]).css({'width': Number(e_w2-2*e_p)+'px','height':e_h+'px','margin-left':e_p+'px','margin-top':Number((d-2*m)/2-e_h/2)+'px'})
	}

	var fb_a_w=40
	var fb_e_h=8

	for(var i=0;i<edg_id.length;i++){
		$('#arrow_'+edg_id[i]+'_fb').css({'margin-top':Number((d-2*m)/2-fb_a_w/2)+'px','border-top': fb_a_w/2+'px solid transparent','border-bottom':fb_a_w/2+'px solid transparent'})
		$('#arrow_'+edg_id_r[i]+'_fb').css({'margin-top':Number((d-2*m)/2-fb_a_w/2)+'px','border-top': fb_a_w/2+'px solid transparent','border-bottom':fb_a_w/2+'px solid transparent'})

		$('#arrow_'+edg_id_r[i]+'_fb').css({'border-right': fb_a_w+'px solid white','margin-left':Number(e_p-8)+'px'})
	}

	for(var i=0;i<edg_id1.length;i++){
		$('#edge_'+edg_id1[i]+'_fb').css({'width': Number(e_w1-2*e_p)+'px','height':fb_e_h+'px','margin-left':e_p+'px','margin-top':Number((d-2*m)/2-fb_e_h/2)+'px'})
		$('#arrow_'+edg_id1[i]+'_fb').css({'border-left': fb_a_w+'px solid white','margin-left':Number(e_p+e_w1-2*e_p-fb_a_w+8)+'px'})
	}
	for(var i=0;i<edg_id2.length;i++){
		$('#edge_'+edg_id2[i]+'_fb').css({'width': Number(e_w2-2*e_p)+'px','height':fb_e_h+'px','margin-left':e_p+'px','margin-top':Number((d-2*m)/2-fb_e_h/2)+'px'})
		$('#arrow_'+edg_id2[i]+'_fb').css({'border-left': fb_a_w+'px solid white','margin-left':Number(e_p+e_w2-2*e_p-fb_a_w+8)+'px'})
	}

	//$('#btn_confirm').css({'top':'43%'})
	//$('.hint').html('?---------?')
}

function click_node(nod){
	if (act_count>=actmax || (!ongoing_mod)||blc_mod[nod]|| Date.now()-t0>last_time){
		return false;
	}
	nod_count[nod]++;
	act_count++;
	$(nod_id[nod]).css('background-color',fad[0]);
	$(nod_id[nod]).html('+')
	setTimeout(cross_fade,100,nod)
	setTimeout(light_fade,fad_time/fad_num,nod_count[nod],nod,1)

	light_generation(nod,trial)
	//change the bonus layout
	var curr="";
	for (var i=0;i<actmax-act_count;i++){
		curr=curr+"<img class='coins' src='static/img/bonus.png'>"
	}
	$('#bonus').html(curr)
	//data record
	myEve[myEve.length]={trial_order:trial,trial_id:str_id[trial],trial_name:str_name[str_id[trial]],
		obj:nod_name[pos[nod]],time:Date.now()-t0,act:1,blc:0,delay:0}
	// console.log(myEve[myEve.length-1])
}

function light_generation(nod,tr){
	if ((!ongoing_mod)||blc_mod[nod]|| Date.now()-t0>last_time||trial!=tr){
		return false;
	}

	for (var i=0;i<struc.length;i++){
		if (struc[i].includes(pos[nod]) && Math.random()<w_pow && Date.now()-t0<last_time){ //!blc_mod[pos.indexOf(i)] &&

			if (myCon=='Reliable'){
				var t=timing_r[Math.floor(Math.random()*timing_num)]
			}else{
				var t=timing_i[Math.floor(Math.random()*timing_num)]
			}

			setTimeout(light_show,t,pos.indexOf(i),tr,t)
		}
	}
}

function light_show(nod,tr,t){
	if ((!ongoing_mod)||blc_mod[nod]|| Date.now()-t0>last_time||trial!=tr){
		return false;
	}
	//light up
	nod_count[nod]++;
	$(nod_id[nod]).css('background-color',fad[0]);
	setTimeout(light_fade,fad_time/fad_num,nod_count[nod],nod,1)
	//continous generation
	light_generation(nod,tr)
	//data_record
	myEve[myEve.length]={trial_order:trial,trial_id:str_id[trial],trial_name:str_name[str_id[trial]],
		obj:nod_name[pos[nod]],time:Date.now()-t0,act:0,blc:0,delay:t}
	//console.log(myEve[myEve.length-1])
}


function light_fade(n_count,nod,fad_count){
	if (n_count==nod_count[nod] && fad_count<fad_num){
		$(nod_id[nod]).css('background-color',fad[fad_count]);
		fad_count++;
		setTimeout(light_fade,fad_time/fad_num,nod_count[nod],nod,fad_count)
	}
}


function cross_fade(nod){
	$(nod_id[nod]).html('')
}


function rightclick_node(nod){
	if ((!ongoing_mod) || Date.now()-t0>last_time){
		return false;
	}
	blc_mod[nod]=1-blc_mod[nod];

	if (blc_mod[nod]){
		//$(nod_id[nod]).css('background-color','#D9D9D9')
		//$(nod_id[nod]).css('background-color','#363636')
		$(nod_id[nod]).css('border-color','#363636')

		$(nod_id[nod]).css('border-width','4px')
		$(nod_id[nod]).html("<div style='height:110%;width:4px;background:#363636;margin-left:auto;margin-right:auto;z-index:1;transform:rotate(45deg);'></div>")


		myEve[myEve.length]={trial_order:trial,trial_id:str_id[trial],trial_name:str_name[str_id[trial]],
		obj:nod_name[pos[nod]],time:Date.now()-t0,act:-1,blc:-1,delay:0}
	}else{
		//$(nod_id[nod]).css('background-color','#D9D9D9')
		$(nod_id[nod]).css('border-color','#ebebeb')

		$(nod_id[nod]).css('border-width','2px')
		$(nod_id[nod]).html('')

		myEve[myEve.length]={trial_order:trial,trial_id:str_id[trial],trial_name:str_name[str_id[trial]],
		obj:nod_name[pos[nod]],time:Date.now()-t0,act:-1,blc:1,delay:0}
	}
	// console.log(myEve[myEve.length-1])
}


function click_edge(edg){
	if (!ongoing_mod){
		return false;
	}
	edg_count[edg]++;
	var e=myClick[(edg_count[edg]-1)% myClick.length]
	if (e==0){//none
		$('#hint_'+edg_id[edg]).css("display", "none")
		$('#edge_'+edg_id[edg]+','+'#arrow_'+edg_id[edg]+','+'#arrow_'+edg_id_r[edg]).css("visibility", "hidden")
	}else if (e==1){//directed
		$('#hint_'+edg_id[edg]).css("display", "none")
		$('#edge_'+edg_id[edg]+','+'#arrow_'+edg_id[edg]).css("visibility", "visible")
		$('#arrow_'+edg_id_r[edg]).css("visibility", "hidden")
	}else if (e==2){//inverse
		$('#hint_'+edg_id[edg]).css("display", "none")
		$('#edge_'+edg_id[edg]+','+'#arrow_'+edg_id_r[edg]).css("visibility", "visible")
		$('#arrow_'+edg_id[edg]).css("visibility", "hidden")
	}else{//loop
		$('#hint_'+edg_id[edg]).css("display", "none")
		$('#edge_'+edg_id[edg]+','+'#arrow_'+edg_id[edg]+','+'#arrow_'+edg_id_r[edg]).css("visibility", "visible")
	}

	var e=new Array
	for (var i=0;i<edg_count.length;i++){
		e[i]=myClick[(edg_count[i]-1)% myClick.length]
		if (edg_count[i]==0){e[i]=0}
	}

	myJud[myJud.length]={trial_order:trial,trial_id:str_id[trial],trial_name:str_name[str_id[trial]],gt_s:struc,gt_e:edges,gt_e_pos:edges_pos,position:pos,online:-1,time:Date.now()-t0,
		ans_s:-1,ans_e:-1,ans_e_pos:e}


	locktime=Date.now()
	$('.edge').css('background','#adacac')
	$('.ar1').css('border-left-color','#adacac')
	$('.ar2').css('border-right-color','#adacac')
	setTimeout(edge_lock,1000)

}

function edge_lock(){
	if (Date.now()-locktime>990){
		$('.edge').css('background','#363636')
		$('.ar1').css('border-left-color','#363636')
		$('.ar2').css('border-right-color','#363636')

		if (Date.now()-t0<last_time){
			data_record(1)
		}
	}
}

function trial_begin(){
	ongoing_mod=1
	t0=Date.now()
	act_count=0
	//lay out
	$('#exp_ins').html('You can click on nodes to explore relationships now...')
	$('#btn_ready').prop('disabled', true)

	if (trial_nod[str_id[trial]]==3){
		draw_net3()
	}else{
		draw_net4()
	}
	$('#pg_game').css({'display':'block'})

	setTimeout(trial_timeout,last_time)
	clock_change()
	bonus_time=Math.floor(Math.random()*last_time)
	setTimeout(bonus_get,bonus_time)
}

function bonus_get(){
	var b=0
	for (var i=0;i<edg_count.length;i++){
		if (e_confirm[i]==edges_pos[i]){
			b=b+1
		}
	}
	myScore[myScore.length]=b
}

function clock_change(){
	var t1=Date.now()-t0
	if ((!ongoing_mod)||t1>last_time){
		$('#clock_container').html('0 s')
		return false;
	}
	if (t1>last_time-10*1000 && t1<last_time-7*1000){
		//$('#clock_container').css('border-color','#dc3545')
		//$('#second_hand').css('background-color','#dc3545')
		$('#clock_container').css('color','#dc3545')
	}
	
	$('#clock_container').html(45-Math.floor(t1/1000)+' s')

	setTimeout(clock_change,50)
}

function trial_timeout(){
	// if (Date.now()-t0<last_time||(!ongoing_mod)){
	// 	return false;
	// }
	for(var i=0;i<nod_id.length;i++){
		//$(nod_id[i]).css('background-color','#D9D9D9')
		$(nod_id[i]).css('border-color','#ebebeb')
		$(nod_id[i]).css('border-width','2px')
		$(nod_id[i]).html('')
	}

	$('.edge').css('background','#363636')
	$('.ar1').css('border-left-color','#363636')
	$('.ar2').css('border-right-color','#363636')

	$('#exp_ins').html('Move onto the next device.')

	ongoing_mod=0

	myTrialInfo[myTrialInfo.length]={trial_order:trial,trial_id:str_id[trial],trial_name:str_name[str_id[trial]],position:pos,begin_time:t0,
									bonus_time:bonus_time,bonus:myScore[myScore.length-1]}

	//console.log(myTrialInfo[myTrialInfo.length-1])

	data_record(0)
}


function data_record(onl){
	//data record
	var e=new Array
	for (var i=0;i<edg_count.length;i++){
		e[i]=myClick[(edg_count[i]-1)% myClick.length]
		if (edg_count[i]==0){e[i]=0}
	}
	
	if (trial_nod[str_id[trial]]==3){var myStr=[[],[],[]]
	}else{var myStr=[[],[],[],[]]}

	var ts=[]
	for (var i=0;i<edg_count.length;i++){
			if (e[i]==1 || e[i]==3){
				ts=myStr[pos[edg_nod[i][1]]]
				ts.push(pos[edg_nod[i][0]])
				myStr[pos[edg_nod[i][1]]]=ts.sort()
			}
			if (e[i]==2 || e[i]==3){
				ts=myStr[pos[edg_nod[i][0]]]
				ts.push(pos[edg_nod[i][1]])
				myStr[pos[edg_nod[i][0]]]=ts.sort()
			}
	}


	//switch edge to orginal edge
	var e_ori=new Array
	for (i=0;i<edg_count.length;i++){
		var a=myStr[edg_nod[i][1]].indexOf(edg_nod[i][0])>-1
		var b=myStr[edg_nod[i][0]].indexOf(edg_nod[i][1])>-1
		if (a && b){
			e_ori[i]=3
		}else if (a && !b){
			e_ori[i]=1
		}else if (b && !a){
			e_ori[i]=2
		}else{
			e_ori[i]=0
		}

		for (var j=0;j<edg_count.length;j++){
			if (edg_nod_pos[i].toString()==edg_nod[j].toString() && isNaN(e[j])){
				e_ori[i]=NaN
			}
		}
	}

	myJud[myJud.length]={trial_order:trial,trial_id:str_id[trial],trial_name:str_name[str_id[trial]],gt_s:struc,gt_e:edges,gt_e_pos:edges_pos,position:pos,online:onl,time:Date.now()-t0,
		ans_s:myStr,ans_e:e_ori,ans_e_pos:e}
	//bonus & feedback
	if (!onl){
		feed_back(e)
	}

	e_confirm=e
}

function feed_back(e){
	// ground truth for each trial, represents by edg
	//using edges_pos
	for (var i=0;i<edg_count.length;i++){
		if (edges_pos[i]==1){
			$('#edge_'+edg_id[i]+'_fb,'+'#arrow_'+edg_id[i]+'_fb').css("display", "block")
		}else if (edges_pos[i]==2){
			$('#edge_'+edg_id[i]+'_fb,'+'#arrow_'+edg_id_r[i]+'_fb').css("display", "block")
		}else if (edges_pos[i]==3){
			$('#edge_'+edg_id[i]+'_fb,'+'#arrow_'+edg_id[i]+'_fb,'+'#arrow_'+edg_id_r[i]+'_fb').css("display", "block")
		}else{
		}

		if(edges_pos[i]==e[i]){
			$('#fr_edge_'+edg_id[i]).css("background-color", "rgb(0,204,0,0.2)")
		}else{
			$('#fr_edge_'+edg_id[i]).css("background-color", "rgb(204,0,0,0.2)")
		}
	}			

	if ($('#Structure').val()!="Select"){
		$('#btn_ready').prop('disabled', false)
	}

}

function trial_next(){
	
	trial=myOpOrder.indexOf($('#Structure').val())


	ongoing_mod=0
	if (trial>=str_pat.length){ //end of the game
		$('#pg_exp').css({'display':'none'})
		bonus_show()
		return false;
	}
	//lay out
	$('#pg_game').css({'display':'none'})
	var curr="";
	for (var i=0;i<actmax;i++){
		curr=curr+"<img class='coins' src='static/img/bonus.png'>"
	}
	$('#bonus').html(curr)
	//$('#clock_container').css('border-color','#363636')
	//$('#second_hand').css('background-color','#363636')
	$('#clock_container').css('color','black')
	$('#clock_container').html('45 s')

	//reset
	if (trial_nod[str_id[trial]]==3){
		nod_count=[0,0,0];edg_count=[0,0,0];blc_mod=[0,0,0];e_confirm=[0,0,0];pos=[0,1,2]
	}else{
		nod_count=[0,0,0,0];edg_count=[0,0,0,0,0,0];blc_mod=[0,0,0,0];e_confirm=[0,0,0,0,0,0];pos=[0,1,2,3]
	}
	//layout reset
	for (i=0;i<nod_id.length;i++){
		$(nod_id[i]).css('background-color','#D9D9D9')
	}
	for (i=0;i<edg_id.length;i++){
		$('#edge_'+edg_id[i]+','+'#arrow_'+edg_id[i]+','+'#arrow_'+edg_id_r[i]).css("visibility", "hidden")
		$('#edge_'+edg_id[i]+'_fb,'+'#arrow_'+edg_id[i]+'_fb,'+'#arrow_'+edg_id_r[i]+'_fb').css("display", "none")
		$('#hint_'+edg_id[i]).css("display", "block")
		$('#fr_edge_'+edg_id[i]).css("background-color", "rgb(191,191,191,0.2)")
	}
	//parameters for the current trial
	struc=str_pat[str_id[trial]]
	pos=shuffle(pos)
	edg_nod_pos=[[pos.indexOf(0),pos.indexOf(1)].sort(),[pos.indexOf(1),pos.indexOf(2)].sort(),[pos.indexOf(0),pos.indexOf(2)].sort(),
				 [pos.indexOf(0),pos.indexOf(3)].sort(),[pos.indexOf(1),pos.indexOf(3)].sort(),[pos.indexOf(2),pos.indexOf(3)].sort()]
	edges=new Array
	edges_pos=new Array
	for (i=0;i<edg_count.length;i++){
		var a=struc[pos[edg_nod[i][1]]].indexOf(pos[edg_nod[i][0]])>-1
		var b=struc[pos[edg_nod[i][0]]].indexOf(pos[edg_nod[i][1]])>-1
		if (a && b){
			edges_pos[i]=3
		}else if (a && !b){
			edges_pos[i]=1
		}else if (b && !a){
			edges_pos[i]=2
		}else{
			edges_pos[i]=0
		}

		var a=struc[edg_nod[i][1]].indexOf(edg_nod[i][0])>-1
		var b=struc[edg_nod[i][0]].indexOf(edg_nod[i][1])>-1
		if (a && b){
			edges[i]=3
		}else if(a && !b){
			edges[i]=1
		}else if (b && !a){
			edges[i]=2
		}else{
			edges[i]=0
		}
	}

	trial_begin()
}


function mychoice(e){
	myCon=$('#Delay').val()

	var t1=$('#Cyclicity').val()
	var t2=$('#Nodes').val()
	var oplis=myOpList[t1][t2]

	if (e==1){
	 $('#Structure').val("Select")
	}

	for (var j=0;j<myOpOrder.length;j++){
		$('#'+myOpOrder[j]).css("display", "none")
	}

	for (j=0;j<oplis.length;j++){
		$('#'+oplis[j]).css("display","block")
	}

	if ($('#Structure').val()=="Select"){
		$('#btn_ready').prop('disabled', true)
	}else{
		$('#btn_ready').prop('disabled', false)
	}

}


function shuffle(a) {
    var j, x, i;
    for (i = a.length - 1; i > 0; i--) {
        j = Math.floor(Math.random() * (i + 1));
        x = a[i];
        a[i] = a[j];
        a[j] = x;
    }
    return a;
}
