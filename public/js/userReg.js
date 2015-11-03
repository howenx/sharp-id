            
    //发送验证码
    function sendCode(){
    	document.getElementById("errorMsg").style.display="none";
    	var imageCode =  document.getElementById("imageCode").value;
    	var phoneNum =  document.getElementById("phoneNum").value;
    	if(imageCode==null||imageCode==""){
    		document.getElementById("errorMsg").style.display="block";
    		document.getElementById("errorMsg").innerHTML="图片验证码不能为空";
    		return false;
    	}
    	var check = /^(1[3578])\d{9}$/;
    	var flag = phoneNum.match(check);
    	if(phoneNum.length!=11){
    		document.getElementById("errorMsg").style.display="block";
    		document.getElementById("errorMsg").innerHTML="电话号码不够11位";
    		return false;
    	}
    	if(flag==null){
    		document.getElementById("errorMsg").style.display="block";
    		document.getElementById("errorMsg").innerHTML="电话号码不符合规则";
    		return false;
    	}
    	document.getElementById("codeButton").disabled=true;
    	$.ajax({  
             type:"POST",
             url:"/regSendCode",
             data:"phoneNum="+phoneNum+"&imageCode="+imageCode,
             success:function(data){  
            	 if(data['if']){
            		 timer = setTimeout('change()',1000);
            		 second = 180;
            	 }else{
            		 document.getElementById("codeButton").disabled=false;
            		 document.getElementById("errorMsg").style.display="block";
             		 document.getElementById("errorMsg").innerHTML=data['back'];
            	 }
    		 }
      	}); 
    	getImageCode();
    }
    //倒计时
    var second;
    var timer;
    function change()
    {
      second--;
     
     if(second>-1)
     {
      	document.getElementById("codeButton").innerHTML="已发送("+second+")";
      	document.getElementById("codeButton").disabled=true;
      	timer = setTimeout('change()',1000);
     }
     else
     {
    	 document.getElementById("codeButton").disabled=false;
         document.getElementById("codeButton").innerHTML="获取验证码";
         clearTimeout(timer);
     }
    }
    
   

   //注册
   function regSubmit(){
    	var phoneNum =  document.getElementById("phoneNum").value;
    	var code =  document.getElementById("code").value;
    	if(phoneNum.length==0){
    		document.getElementById("errorMsg").style.display="block";
    		document.getElementById("errorMsg").innerHTML="电话号码不能为空";
    		return false;
    	}
    	if(code.length!=6){
    		document.getElementById("errorMsg").style.display="block";
    		document.getElementById("errorMsg").innerHTML="验证码应为6位数字";
    		return false;
    	}
    	$.ajax({
                     type:"POST",
                     url:"/regSubmit",
                     data:$("#regForm").serialize(),
                     success:function(data){
                    	 if(data['if']){
                    		document.getElementById("completeForm").style.display="block";
                    	 }else{
                    	 	document.getElementById("errorMsg").style.display="block";
                    	 	document.getElementById("errorMsg").innerHTML=data['back'];
                    	 }
            		 }
              	});

    	
    }
   
   function onPhone(){
		document.getElementById("errorMsg").style.display="none";
	}
   function toComplete(){
		document.getElementById("toComplete").submit();
	}

   function getImageCode() {
		document.getElementById("imageCodeImage").src="/getImageCodes/"+Math.round(Math.random()*1000000);
	}