

//发送验证码
function sendCode(){
	var phoneNum =  document.getElementById("phoneNum").value;
	var userId =  document.getElementById("userId").value;
	document.getElementById("codeMsg").innerHTML="";
	var check = /^(1[3578])\d{9}$/;
	var flag = phoneNum.match(check);
	if(phoneNum.length!=11){
		document.getElementById("codeMsg").innerHTML="电话号码不够11位";
		return false;
	}
	if(flag==null){
		document.getElementById("codeMsg").innerHTML="电话号码不符合规则";
		return false;
	}
	$.ajax({  
         type:"POST",
         url:"/detailSendCode",
         data:"phoneNum="+phoneNum+"&userId="+userId,
         success:function(data){  
        	if(data['if']){
        		document.getElementById("codeButton").onclick="";
        		document.getElementById("backCode").innerHTMl=data['code'];
        		second = 180;
        		timer = setTimeout('change()',1000);
        	}else{
        		document.getElementById("codeMsg").innerHTML=data['back'];
        	}
		 }
  	}); 
}
//计时器
var second;
var timer;
function change()
{
 	second--;
 if(second>-1){
  	document.getElementById("codeButton").innerHTML="已发送("+second+")";
  	document.getElementById("codeButton").href="javascript:void(0)";
  	timer = setTimeout('change()',1000);//调用自身实现
 }else{
	 document.getElementById("codeButton").href="javascript:sendCode()";
     document.getElementById("codeButton").innerHTML="获取短信验证码";
     clearTimeout(timer);
 }
}

//绑定手机
function changePhone(){
	var userId = document.getElementById("userId").value;
	var codeNew = document.getElementById("codeNew").value;
	var codeOld = document.getElementById("codeOld").value;
	var phoneNum = document.getElementById("phoneNum").value;
	if(codeOld.length==0||codeNew.length==0||phoneNum.length==0){
		document.getElementById("codeMsg").innerHTML="验证码 电话号码不能为空";
		return false;
	}
	else{
		$.ajax({  
	        type:"POST",
	        url:"/changePhone",
	        data:"phoneNum="+phoneNum+"&userId="+userId+"&oldCode="+codeOld+"&newCode="+codeNew,
	        success:function(data){  
	        	document.getElementById("codeNew").value="";
	        	document.getElementById("codeOld").value="";
	        	document.getElementById("phoneNum").value="";
		       	document.getElementById("codeMsg").innerHTML=data['back'];
		       	document.getElementById("codeButton").href="javascript:sendCode()";
		        document.getElementById("codeButton").innerHTML="获取短信验证码";
		        clearTimeout(timer);
			 }
	 	});
	}
}
//修改密码
function chagePassword(){
	var oldPassword = document.getElementById("oldPassword").value;
	var newPassword = document.getElementById("newPassword").value;
	var againPassword = document.getElementById("againPassword").value;
	var userId = document.getElementById("userId").value;
	if(oldPassword.length==0){
		document.getElementById("passwordMsg").innerHTML="原密码不能为空！";
		return false;
	}
	if(oldPassword==newPassword){
		document.getElementById("passwordMsg").innerHTML="新旧密码不能相同！";
		return false;
	}
	if(newPassword.length==0){
		document.getElementById("passwordMsg").innerHTML="新密码不能为空！";
		return false;
	}	
	if(newPassword!=againPassword){
		document.getElementById("passwordMsg").innerHTML="两次输入密码不一致！";
		return false;
	}
	$.ajax({  
        type:"POST",
        url:"/changePassword",
        data:"newPassword="+newPassword+"&againPassword="+againPassword+"&userId="+userId+"&oldPassword="+oldPassword,
        success:function(data){ 
        	document.getElementById("passwordMsg").innerHTML=data['back'];
        	if(data['if']){
        		document.getElementById("newPassword").value="";
        		document.getElementById("againPassword").value="";
        		document.getElementById("oldPassword").value="";
        	}
		 }
 	}); 
}

function sendEmail(){
	var email = document.getElementById("userEmail").value;
	var userId = document.getElementById("userId").value;
	if(email==null||email==""){
		document.getElementById("emailMsg").innerHTML="账户为空不能激活";
		return false;
	}
	$.ajax({
        type:"POST",
        url:"/sendEmail",
        data:"email="+email+"&id="+userId,
        success:function(data){  
	       	document.getElementById("emailMsg").innerHTML=data['back'];
		 }
 	});
}

